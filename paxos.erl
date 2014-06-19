
-module(paxos).
-export([start/0,pre_loop/0,loop/9,command_line/0]).

start() ->
   io:fwrite("let's start, command line process is cmd"),
   register(cmd,spawn(paxos,command_line,[])),
   % upon event init
   Procs = [spawn(paxos,pre_loop,[]) || X<-lists:seq(0,6)],
   % send the Procs
   lists:foreach(fun(N) -> N ! {other_procs,Procs} end,Procs).

yes_no(true,ForTrue,_)   -> ForTrue;
yes_no(false,_,ForFalse) -> ForFalse.

command_line() ->
   receive
      {setN} -> io:fwrite("Hello, welcome to talk to paxos!");
      _      -> io:fwrite("Not recognized command")
   end,
   command_line().

pre_loop() ->
   ok.

% actors/states: proposer, acceptor, learner
% N is all procs
% readList is of type [Pid,{}]
loop(Ac,N,Rank,T,Prepts,A={Ats,Av},P={Pts,Pv},ReadLs,Acks) ->
   receive
      % set the other processes
      {other_procs,Procs} ->
         loop(Ac,Procs,Rank,T,Prepts,A,P,ReadLs,Acks);
      % phase 1 propose
      {propose,V} ->
         % trigger < beb, Broadcast | [Prepare, pts, t] >
         loop(Ac,
              N,
              Rank,
              T+1,
              Prepts,
              A,
              {(T+1)*length(ReadLs)*Rank,V},
              [undefined],
              0);
       
      % phase 1 deliver prepare
      {Q,{prepare,Ts,T_}} ->
         NewT  = max(T,T_)+1,
         NewTs = case Ts < Prepts of
                    true  ->
                       Q ! {nack,Ts,T},
                       Prepts;
                    false ->
                       Q ! {prepare_ack,Ats,Av,Ts,T},
                       Ts
                 end,
         loop(Ac,N,Rank,T,NewTs,A,P,ReadLs,Acks);
      
      % phase 2 nack
      {Q,{nack,Pts_,T_}} ->
         NewT   = max(T,T_)+1, % increase logical clock
         NewPts = case Pts_ == Pts of
                     true ->
                        Ac ! abort,
                        {0,Pv};
                     false ->
                        P
                  end,
         loop(Ac,N,Rank,NewT,Prepts,A,NewPts,ReadLs,Acks);
     
      %% fix below
      
      % todo fix
      {deliver,Q,{prepare_ack,Ts,V,Pts_,T_}} ->
         NewT = max(T,T_)+1,
         case Pts_ == Pts of
            true ->
               % replace list of Q
               case (length(ReadLs) > (length(N) / 2)) of
                  true ->
                     {NewTs,NewV} = lists:max(ReadLs),
                     NewPv = yes_no(Ts/=0,NewV,Pv),
                     NewReadLs = [],
                     % todo broadcast accept
                     loop(Ac,N,Rank,NewT,Prepts,{Ats,Av},{Pts,NewV},ReadLs,Acks)
               end
         end,
         loop(Ac,N,Rank,T,Prepts,A,P,ReadLs,Acks);
       
      {deliver,Q,{accept,Ts,V,T_}} ->
         NewT = max(T,T_)+1,
         case Ts < Prepts of
            false ->
               % ats = prepts = ts
               % av  = v
               %trigger
               loop(Ac,N,Rank,NewT,Ts,{Ts,V},P,ReadLs,Acks);
            true ->
               % trigger
               loop(Ac,N,Rank,NewT,Prepts,A,{0,Pv},ReadLs,Acks)
         end;
   
      % loop(N,Rank,T,Prepts,A={Ats,Av},P={Pts,Pv},ReadLs,Acks) ->
      {deliver,Q,{accept_ack,Pts_,T_}} ->
         NewT = max(T,T_)+1,
         case Pts_ == Pts of
            true ->
               NewAcks = Acks + 1,
               case (NewAcks > N / 2) of
                  true ->
                     NewPts = 0,
                     % trigger ac return
                     loop(Ac,N,Rank,NewT,T,A,{NewPts,Pv},ReadLs,NewAcks);
                  false ->
                     loop(Ac,N,Rank,NewT,T,A,P,ReadLs,NewAcks)
               end;
            false ->
               loop(Ac,N,Rank,NewT,T,A,P,ReadLs,Acks)
         end,
         io:fwrite("Monkey")
   end.
