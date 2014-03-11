% TODO: now only for one process, implement for x procs
% TODO: test it
% TODO: now only phase 1, implement phase 2
-module(paxos).
-export([start/0]).

start() ->
   io:fwrite("let's start"),
   % upon event init
   Pid = spawn(paxos,
               loop,
               [1,0,0,{0,undefined},{0,undefined},[undefined],0]
              ).

% actors/states: proposer, acceptor, learner
loop(Rank,T,Prepts,A={Ats,Av},P={Pts,Pv},ReadLs,Acks) ->
   receive
       % phase 1 propose
       {propose,V} ->
           % trigger < beb, Broadcast | [Prepare, pts, t] >
           loop(Rank,T+1,Prepts,A,{(T+1)*length(ReadLs)*Rank,V},[undefined],0);
       
       % phase 1 deliver prepare
       {deliver,Q,{prepare,Ts,T_}} ->
           NewT = max(T,T_)+1,
           case Ts < Prepts of
               false ->
                   % broadcast
                   loop(Rank,NewT,Ts,A,P,ReadLs,Acks);
               true ->
                   % broadcast
                   loop(Rank,NewT,Prepts,A,P,ReadLs,Acks)
           end;
           
       % phase 2 deliver nack
       {deliver,Q,{nack,Pts_,T_}} ->
           NewT = max(T,T_)+1,
           case Pts == Pts_ of
               false ->
                   loop(Rank,NewT,Prepts,A,P,ReadLs,Acks);
               true ->
                   loop(Rank,NewT,Prepts,A,{0,Pv},ReadLs,Acks) % Pts = 0
                   % trigger abort
           end;
           
       % todo fix
       {deliver,Q,{prepare_ack,Ts,V,Pts_,T_}} ->
           NewT = max(T,T_)+1,
           undefined;
       
       {deliver,Q,{accept,Ts,V,T_}} ->
           NewT = max(T,T_)+1,
           case Ts < Prepts of
               false ->
                   % ats = prepts = ts
                   % av  = v
                   %trigger
                   loop(Rank,NewT,Ts,{Ts,V},P,ReadLs,Acks);
               true ->
                   % trigger
                   loop(Rank,NewT,Prepts,A,{0,Pv},ReadLs,Acks)
           end;
       
       % todo fix
       {deliver,Q,{accept_ack,Pts_,T_} ->
           NewT = max(T,T_)+1,
           undefined
           
   end.