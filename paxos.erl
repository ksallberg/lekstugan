%%% 
%%% The paxos algorithm for abortable consensus
%%% 
%%% Algorithm from the "Advanced Distributed Systems"
%%% course from KTH...
%%% 

-module(paxos).
-export([start/0,loop/9,command_line/1]).

start() ->
   io:fwrite("let's start, command line process is cmd~n"),
   Cmd_name = cmd,
   register(Cmd_name,spawn(paxos,command_line,[[]])),
   % upon event init
   Procs = [spawn(paxos,loop,[Cmd_name, % AC
                              [],       % N
                              X,        % Rank... id number
                              0,        % Time
                              0,        % Prepts
                              {0,nil},    % A, {0,nil}
                              {0,nil},    % P, {0,nil}
                              [],       % ReadLs
                              0         % Acks
                             ]) || X <- lists:seq(0,25)],
   % send the Procs
   lists:foreach(fun(N) -> N ! {other_procs,Procs} end,Procs),
   Cmd_name ! {other_procs,Procs},
   % tell one process to propose 100
   lists:nth(4, Procs) ! {propose,100},
   % tell another to propose 200
   lists:nth(8, Procs) ! {propose,200},
   % and someone to propose 300
   lists:nth(12,Procs) ! {propose,300}.

command_line(N) ->
   receive
      {other_procs,Procs} -> io:fwrite("Cmd received other_procs\n"),
                             command_line(Procs);
      {propose,Val}       -> io:fwrite("Val proposed: ~p~n",[Val]),
                             lists:nth(1,N) ! {propose,Val}; % send to paxos
      {return,Pv}         -> io:fwrite("Consensus achieved: ~p~n",[Pv]);
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
         NewPrepts = case Ts < Prepts of
                        true  ->
                           % trigger ⟨ q, [Nack, ts, t] ⟩;
                           Q ! {self(),{nack,Ts,NewT}},
                           Prepts;
                        false ->
                           % trigger ⟨ q, [PrepareAck, ats, av, ts, t] ⟩;
                           Q ! {self(),{prepare_ack,Ats,Av,Ts,T}},
                           Ts
                     end,
         loop(Ac,N,Rank,T,NewPrepts,A,P,ReadLs,Acks);
      
      % phase 2 nack
      {_,{nack,Pts_,T_}} ->
         NewT   = max(T,T_)+1, % increase logical clock
         NewPts = case Pts_ == Pts of
                     true ->
                        % trigger ⟨ ac, Abort ⟩
                        Ac ! abort,
                        {0,Pv};
                     false ->
                        P
                  end,
         loop(Ac,N,Rank,NewT,Prepts,A,NewPts,ReadLs,Acks);
      
      {Q,{prepare_ack,Ts,V,Pts_,T_}} ->
         NewT = max(T,T_)+1,
         {NewA,NewPv_,NewReadLs} = 
            case Pts_ == Pts of
               true ->
                  NewReadLs_ = lists:keystore(Q,1,ReadLs,{Q,Ts,V}),
                  % replace list of Q
                  case (length(NewReadLs_) > (length(N) / 2)) of
                     true ->
                        {_,NewTs,NewV} = highest(NewReadLs_),
                        NewPv = case NewTs/=0 of
                                   true  -> NewV;
                                   false -> Pv
                                end,
                        % trigger ⟨ Broadcast | [Accept,pts,pv,t] ⟩;
                        tell_peers({self(),{accept,Pts,NewPv,NewT}},N),
                        {{NewTs,NewV},NewPv,[]}; % NewReadLs empty
                     false ->
                        {A,Pv,NewReadLs_}
                  end;
               false ->
                  {A,Pv,ReadLs}
            end,
         loop(Ac,N,Rank,NewT,Prepts,NewA,{Pts,NewPv_},NewReadLs,Acks);
       
      {Q,{accept,Ts,V,T_}} ->
         NewT = max(T,T_)+1,
         {NewPrepts,NewA} = case Ts < Prepts of
            true ->
               % trigger ⟨ q, [Nack, ts, t] ⟩;
               Q ! {self(),{nack,Ts,NewT}},
               {Prepts,A};
            false ->
               NewPrepts_ = Ts,
               NewAts     = Ts,
               NewAv      = V,
               % trigger ⟨ q, [AcceptAck, ts, t] ⟩;
               Q ! {self(),{accept_ack,Ts,NewT}},
               {NewPrepts_,{NewAts,NewAv}}
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
