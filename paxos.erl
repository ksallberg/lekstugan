%%% 
%%% The paxos algorithm for abortable consensus
%%% 
%%% Algorithm from the "Advanced Distributed Systems"
%%% course from KTH...
%%% 

-module(paxos).
-export([start/0,loop/9,command_line/0]).
-record(readls_mem,{pid,ts,v}).

start() ->
   io:fwrite("let's start, command line process is cmd"),
   Cmd_name = cmd,
   register(Cmd_name,spawn(paxos,command_line,[])),
   % upon event init
   Procs = [spawn(paxos,loop,[Cmd_name, % AC
                              [],       % N
                              X,        % Rank... id number
                              0,        % Time
                              0,        % Prepts
                              {0,nil},  % A
                              {0,nil},  % P
                              [],       % ReadLs
                              0         % Acks
                             ]) || X<-lists:seq(0,6)],
   % send the Procs
   lists:foreach(fun(N) -> N ! {other_procs,Procs} end,Procs).

command_line() ->
   receive
      {setN,Val}  -> io:fwrite("Value proposed:\n"),
                     io:format("%s\n",Val);
      {return,Pv} -> _ = Pv,
                     io:fwrite("Return received.\n");
      abort       -> io:fwrite("Aborted!\n");
      _           -> io:fwrite("Not recognized command.\n")
   end,
   command_line().

% make a broadcast
tell_peers(Procs,msg) -> lists:foreach(fun(N) -> N ! msg end,Procs).

% get the highest process
% @type ReadLs: [{Pid,{Ts,V}}]
highest(ReadLs) -> lists:foldl(fun(A,B) ->
                                  case A#readls_mem.ts > B#readls_mem.ts of
                                     true  -> {A#readls_mem.ts,A#readls_mem.v};
                                     false -> {B#readls_mem.ts,B#readls_mem.v}
                                  end
                               end,
                               lists:nth(1,ReadLs), % head of ls
                               ReadLs).

% actors/states: proposer, acceptor, learner
% N is all procs
% readList is of type [Pid,{}]
loop(Ac,N,Rank,T,Prepts,A={Ats,Av},P={Pts,Pv},ReadLs,Acks) ->
   receive
      % set the other processes
      {other_procs,Procs} ->
         io:fwrite("received other_procs\n"),
         loop(Ac,Procs,Rank,T,Prepts,A,P,ReadLs,Acks);
   
      % phase 1 propose
      {propose,V} ->
         NewT      = T+1,
         NewPts    = NewT * length(N) + Rank,
         NewPv     = V,
         NewReadLs = [],
         NewAcks   = 0,
         % trigger < beb, Broadcast | [Prepare, pts, t] >
         tell_peers({self(),{prepare,NewPts,NewT}},N),
         loop(Ac,N,Rank,NewT,Prepts,A,{NewPts,NewPv},NewReadLs,NewAcks);
      
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
                        {NewTs,NewV} = highest(NewReadLs_),
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
               NewAcks = Acks + 1,
               case (NewAcks > N / 2) of
                  true ->
                     NewPts = 0,
                     % trigger ⟨ ac, Return | pv ⟩;
                     Ac ! {return,Pv},
                     {{NewPts,Pv},NewAcks};
                  false ->
                     {P,NewAcks}
               end;
            false ->
               {P,Acks}
         end,
         loop(Ac,N,Rank,NewT,T,A,NewP,ReadLs,NA)
   end.
