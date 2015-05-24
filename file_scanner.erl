%% A file searching program, printing all files matching a given pattern to
%% the terminal. It works concurrently with message passing (using a given
%% number of spawned processes).

-module(file_scanner).
-export([main/3]).

% usage: scanner:main(files,X,Y).
%    where X is amount of processes
%          Y us pattern like ".java"
% populate a process with
main(Path,Processes,Pattern) ->
   % create the process holding directories
   % give it a list of directories to scan
   % a list scanned files matching
   % and a variable that knows when all dirs are entered into list 1
   Workers = [spawn(fun() -> worker(Pattern) end) ||
                          _ <- lists:seq(1,Processes)],
   Dirs = spawn(fun() -> dir_list([],Workers,[]) end),
   scan(Dirs,Path),
   % tell all the DirActor that all directories are in there now
   Dirs ! {all_dirs_read},
   io:fwrite("").

% scan through the given path for directories
% send all directories to the DirActor which will
% distribute them to workers
scan(DirActor,Path) ->
   case filelib:is_dir(Path) of
      false -> [];
      true  -> {ok,Listing} = file:list_dir(Path),
               DirActor ! {add_dir,Path},
               [scan(DirActor,filename:join(Path,Name)) || Name <- Listing]
   end.

% a function that will be run by a process that
dir_list(Matches,Workers,DeadWorkers) ->
   receive
      {dead,DeadW} ->
         case DeadW == length(Workers) of
            true ->
               print_all(Matches);
            false ->
               ok
         end,
         dir_list(Matches,Workers,DeadW);
      {add_dir,Dir} ->
         % get a random worker
         RandWorker = lists:nth(random:uniform(length(Workers)),Workers),
         RandWorker ! {folder,Dir,self()},
         dir_list(Matches,Workers,DeadWorkers);
      {add_file,File} ->
         dir_list(Matches++[File],Workers,DeadWorkers);
      {all_dirs_read} ->
         % tell all workers they can die
         kill_workers(Workers,self(),0),
         dir_list(Matches,Workers,DeadWorkers)
   end.

% recursively kill all workers
kill_workers([],_,_) -> ok;
kill_workers([X|Xs],Caller,Dead) ->
   X ! {die,Caller,Dead},
   kill_workers(Xs,Caller,Dead+1).

% the function that a worker process will be running
worker(Pattern) ->
   receive
      {folder,Folder,Caller} ->
         {ok,FolderContent} = file:list_dir(Folder),
         Files = [Folder ++ File || File <- FolderContent,
                          (filelib:is_dir(File) == false) and
                          wrap_regexp(File,Pattern) == true
                 ],
         send_files(Files,Caller),
         worker(Pattern);
      {die,From,DeadWorkers} ->
         From ! {dead,DeadWorkers+1}
   end.

% send a list of file names to
% a given process
send_files([],_) -> ok;
send_files([X|Xs],SendTo) ->
   SendTo ! {add_file,X},
   send_files(Xs,SendTo).

% perform the reg exp checkup and
% return true or false
wrap_regexp(Str,Exp) ->
   {ok,MP} = re:compile(Exp),
   case re:run(Str,MP) of
      nomatch -> false;
      _ -> true
   end.

% debug print everything in a list
print_all([]) -> ok;
print_all([X|Xs]) -> io:fwrite(X),
                     io:fwrite("\n"),
                     print_all(Xs).
