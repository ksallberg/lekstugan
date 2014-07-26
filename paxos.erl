%%%
%%% The paxos algorithm for abortable consensus
%%%
%%% Algorithm from the "Advanced Distributed Systems"
%%% course from KTH...
%%%

-module(paxos).
-export([start/0,loop/9,command_line/1]).

-define(CMD_NAME,cmd).

start() ->
   io:fwrite("let's start, command line process is cmd~n"),
   register(?CMD_NAME,spawn(paxos,command_line,[[]])),
   % upon event init
   Procs = [spawn(paxos,loop,[?CMD_NAME, % AC
                              [],        % N
                              X,         % Rank... id number
                              0,         % Time
                              0,         % Prepts
                              {0,nil},   % A
                              {0,nil},   % P
                              [],        % ReadLs
                              0          % Acks
                             ]) || X <- lists:seq(0,25)],
   % send the Procs
   lists:foreach(fun(N) -> N ! {other_procs,Procs} end,Procs),
   ?CMD_NAME ! {other_procs,Procs},
   % tell one process to propose 100
   lists:nth(4, Procs) ! {propose,100},
   % tell another to propose 200
   lists:nth(8, Procs) ! {propose,200},
   % and someone to propose 300
   lists:nth(12,Procs) ! {propose,300},
   % rank.. 1
   lists:nth(1,Procs)  ! {propose,400}.

command_line(N) ->
   receive
      {other_procs,Procs} -> io:fwrite("Cmd received other_procs\n"),
                             command_line(Procs);
      {propose,Val}       -> io:fwrite("Val proposed: ~p~n",[Val]),
                             lists:nth(1,N) ! {propose,Val}; % send to paxos
      {return,Pv}         -> io:fwrite("Consensus achieved: ~p~n",[Pv]),
                             unregister(?CMD_NAME);
      abort               -> io:fwrite("Aborted!\n");
      _                   -> io:fwrite("Not recognized command.\n")
   end,
   command_line(N).

% make a broadcast
tell_peers(Msg,Procs) -> lists:foreach(fun(N) -> N ! Msg end,Procs).

% get the highest process
% @type ReadLs: [{Pid,{Ts,V}}]
highest(ReadLs) -> lists:nth(1,lists:keysort(2,ReadLs)).

% actors/states: proposer, acceptor, learner
% N is all procs
% readList is of type [Pid,{}]
loop(Ac,N,Rank,T,Prepts,A={Ats,Av},P={Pts,Pv},ReadLs,Acks) ->
   receive
      % set the other processes
      {other_procs,Procs} ->
         io:fwrite("received other_procs~n"),
         loop(Ac,Procs,Rank,T,Prepts,A,P,ReadLs,Acks);

      % phase 1 propose
      {propose,V} ->
         io:fwrite("Paxos; received propose~n"),
         NewT      = T+1,
         NewPts    = NewT * length(N) + Rank,
         NewPv     = V,
         NewAcks   = 0,
         % trigger < beb, Broadcast | [Prepare, pts, t] >
         tell_peers({self(),{prepare,NewPts,NewT}},N),
         loop(Ac,N,Rank,NewT,Prepts,A,{NewPts,NewPv},[],NewAcks);

      % phase 1 deliver prepare
      {Q,{prepare,Ts,T_}} ->
         NewT  = max(T,T_)+1,
         case Ts < Prepts of
            true  ->
               % trigger ⟨ q, [Nack, ts, t] ⟩;
               Q ! {self(),{nack,Ts,NewT}},
               NewPrepts = Prepts;
            false ->
               % trigger ⟨ q, [PrepareAck, ats, av, ts, t] ⟩;
               Q ! {self(),{prepare_ack,Ats,Av,Ts,T}},
               NewPrepts = Ts
         end,
         loop(Ac,N,Rank,T,NewPrepts,A,P,ReadLs,Acks);

      % phase 2 nack
      {_,{nack,Pts_,T_}} ->
         NewT   = max(T,T_)+1, % increase logical clock
         case Pts_ == Pts of
            true ->
               % trigger ⟨ ac, Abort ⟩
               Ac ! abort,
               NewPts = {0,Pv};
            false ->
               NewPts = P
         end,
         loop(Ac,N,Rank,NewT,Prepts,A,NewPts,ReadLs,Acks);

      % Note NewReadLs is defined differently in all case
      % branches and is still accessible in the loop function call
      {Q,{prepare_ack,Ts,V,Pts_,T_}} ->
         NewT = max(T,T_)+1,
         case Pts_ == Pts of
            true ->
               AddedReadLs = lists:keystore(Q,1,ReadLs,{Q,Ts,V}),
               % replace list of Q
               case (length(AddedReadLs) > (length(N) / 2)) of
                  true ->
                     {_,NewTs,NewV} = highest(AddedReadLs),
                     NewPv = case NewTs/=0 of
                                true  -> NewV;
                                false -> Pv
                             end,
                     % trigger ⟨ Broadcast | [Accept,pts,pv,t] ⟩;
                     tell_peers({self(),{accept,Pts,NewPv,NewT}},N),
                     NewReadLs = [],
                     {NewA,NewPv_} = {{NewTs,NewV},NewPv};
                  false ->
                     NewReadLs = AddedReadLs,
                     {NewA,NewPv_} = {A,Pv}
               end;
            false ->
               NewReadLs = ReadLs,
               {NewA,NewPv_} = {A,Pv}
         end,
         loop(Ac,N,Rank,NewT,Prepts,NewA,{Pts,NewPv_},NewReadLs,Acks);

      {Q,{accept,Ts,V,T_}} ->
         NewT = max(T,T_)+1,
         case Ts < Prepts of
            true ->
               % trigger ⟨ q, [Nack, ts, t] ⟩;
               Q ! {self(),{nack,Ts,NewT}},
               NewPrepts = Prepts,
               NewA = A;
            false ->
               NewPrepts = Ts,
               NewAts    = Ts,
               NewAv     = V,
               % trigger ⟨ q, [AcceptAck, ts, t] ⟩;
               Q ! {self(),{accept_ack,Ts,NewT}},
               NewA = {NewAts,NewAv}
         end,
         loop(Ac,N,Rank,NewT,NewPrepts,NewA,P,ReadLs,Acks);

      % {Q, {accept_ack,Pts',T'}}
      {_,{accept_ack,Pts_,T_}} ->
         NewT = max(T,T_)+1,
         {NewP,NA} = case Pts_ == Pts of
            true ->
               NewAcks = Acks+1,
               io:fwrite("accept_ack true ~p~n",[NewAcks]),
               case (NewAcks > length(N) / 2) of
                  true ->
                     NewPts = 0,
                     % trigger ⟨ ac, Return | pv ⟩;
                     Ac ! {return,Pv},
                     {{NewPts,Pv},NewAcks};
                  false ->
                     {P,NewAcks}
               end;
            false ->
               io:fwrite("accept_ack false ~n"),
               {P,Acks}
         end,
         loop(Ac,N,Rank,NewT,T,A,NewP,ReadLs,NA)
   end.
