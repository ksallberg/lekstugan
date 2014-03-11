% TODO: now only for one process, implement for x procs
% TODO: test it
% TODO: now only phase 1, implement phase 2
% TODO: clean up some weird things
-module(paxos).
-export([start/0]).

start() ->
   io:fwrite("let's start"),
   % upon event init
   Pid = spawn(paxos,
               loop,
               [not_instantiated]
              ).

yes_no(true,ForTrue,_)   -> ForTrue;
yes_no(false,_,ForFalse) -> ForFalse.

% this is before a paxos process is instantiated, 
% it will just wait to be kicked off
loop(not_instantiated) ->
    receive
        % initialize values
       {init,{N,_T_,Prepts_,A_,P_,ReadLs_,Acks_}} ->
           loop(N,Rank,T_,Prepts_,A_,P_,ReadLs_,Acks_)
    end;
% actors/states: proposer, acceptor, learner
% N is all procs
% readList is of type [Pid,{}]
loop(N,Rank,T,Prepts,A={Ats,Av},P={Pts,Pv},ReadLs,Acks) ->
   receive
       % phase 1 propose
       {propose,V} ->
           % trigger < beb, Broadcast | [Prepare, pts, t] >
           loop(N,Rank,T+1,Prepts,A,{(T+1)*length(ReadLs)*Rank,V},[undefined],0);
       
       % phase 1 deliver prepare
       {deliver,Q,{prepare,Ts,T_}} ->
           NewT = max(T,T_)+1,
           case Ts < Prepts of
               false ->
                   % broadcast
                   loop(N,Rank,NewT,Ts,A,P,ReadLs,Acks);
               true ->
                   % broadcast
                   loop(N,Rank,NewT,Prepts,A,P,ReadLs,Acks)
           end;
           
       % phase 2 deliver nack
       {deliver,Q,{nack,Pts_,T_}} ->
           NewT = max(T,T_)+1,
           case Pts == Pts_ of
               false ->
                   loop(N,Rank,NewT,Prepts,A,P,ReadLs,Acks);
               true ->
                   loop(N,Rank,NewT,Prepts,A,{0,Pv},ReadLs,Acks) % Pts = 0
                   % trigger abort
           end;
           
       % todo fix
       {deliver,Q,{prepare_ack,Ts,V,Pts_,T_}} ->
           NewT = max(T,T_)+1,
           case Pts_ == Pts of
               true ->
                   % replace list of Q
                   case length(ReadLs) > length(N) / 2 of
                       true ->
                           (NewTs,NewV) = highest(ReadLs),
                           NewPv = yes_no(ts/=0,NewV,Pv),
                           NewReadLs = [],
                           % todo broadcast accept
                           loop(N,Rank,NewT,Prepts,{Ats,Av},{Pts,NewV},ReadLs,Acks)
                   end
           end,
           loop(N,Rank,T,Prepts,A,P,ReadLs,Acks);
       
       {deliver,Q,{accept,Ts,V,T_}} ->
           NewT = max(T,T_)+1,
           case Ts < Prepts of
               false ->
                   % ats = prepts = ts
                   % av  = v
                   %trigger
                   loop(N,Rank,NewT,Ts,{Ts,V},P,ReadLs,Acks);
               true ->
                   % trigger
                   loop(N,Rank,NewT,Prepts,A,{0,Pv},ReadLs,Acks)
           end;
       
       % todo fix
       {deliver,Q,{accept_ack,Pts_,T_} ->
           NewT = max(T,T_)+1,
           case Pts_ == Pts of
               true ->
                   NewAcks = Acks + 1,
                   
                   case NewAcks > N / 2 of
                       true ->  NewPts = 0,
                                % trigger ac return
                                loop(N,Rank,NewT,Ts,A,{NewPts,Pv},ReadLs,NewAcks);
                       false -> loop(N,Rank,NewT,Ts,A,P,ReadLs,NewAcks)
                   end;
                   
               false -> loop(N,Rank,NewT,Ts,A,P,ReadLs,Acks)
           end
   end.