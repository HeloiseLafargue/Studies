
module GreenThreads =
  struct
    (* à compléter/modifier *)
    type program = unit -> unit ;;

    type rec result = 
      | Exit (* fin de l'exécution *)
      | Yield of continuation (* un processus rend la main, de type continuation *)
      | Fork of program * continuation (* rendre la main et lancer un programme *)
      and continuation = unit -> result ;;
    
    let prompt : result Delimcc.prompt = Delimcc.new_prompt() ;;

    let yield () : unit = Delimcc.shift prompt (fun k -> Yield k) ;;

    let fork (prog : program) : unit = Delimcc.shift prompt (fun k -> Fork(prog, k)) ;;

    let exit () : unit = Delimcc.shift prompt (fun _ -> Exit) ;; (* définition de l'exit via le shift de l'API, c'est comme un raise mais il donne la continuation aussi, la fonction attend une continuation, mais on s'en fiche -> exit *)
  
    let scheduler (parent_prog : program) : = 
      
      (* file des continuations (= processus en cours) *)
      let waiting : continuation Queue.t = Queue.create () in
      
      (* démarre le programme [prog] *)
      let rec spawn (prog : program) : unit =
        handle(Delimcc.push_prompt promt (fun () -> prog(); Exit)) 
        (* dans le handle, type result :
           [prog] renvoie un [unit] mais un processus renvoie un [result], 
           donc ajout d'[Exit] qui correspond à la fin normale *)
        (* [push_prompt] capture les éventuelles "interruptions", ou alors laisse passer
           le [Exit] qui correspond à la fin "normale" *)
      
        (* [handle] stipule quoi faire en sortie de processus (pour cause de [shift] ou
           de fin "normale") *)
      and handle : result -> unit = function
        | Exit -> if Queue.is empty waiting then () (* si rien dans la file d'attente, unit *)
                  else handle (Queue.pop waiting ()) (* sinon on passe au suivant de la file *)
        | Yield k -> Queue.push k waiting; (* ajouter la continuation dans la file, si on veut la reprendre après *)
                    handle (Queue.pop waiting ()) (* pour passer au suivant *)
        | Fork prog -> Queue.push k waiting; (* ajouter la continuation dans la file, si on veut la reprendre après *)
                      spawn prog (* démarre le programme suivant *)
      in spawn parent_prog;;
    
    let ping () =
        for i = 1 to 10 do
          Delimcc.print_endline "ping";
          yield (); (* rendre la main après chaque affichage *)
        done ;;
    
    let pong () =
        for i = 1 to 10 do
          Delimcc.print_endline "pong";
          yield (); (* rendre la main après chaque affichage *)
        done ;;

    let ping_pong () =
      begin
        fork ping ();
        fork pong ();
        Exit (* fin de l'éxécution *)
      end ;;

    let main () = scheduler(ping_pong);;

  end

module type Channel =
  sig (* renvoie des fonctions "write" et "read" *)
    val create : unit -> ('a -> unit) * (unit -> 'a)
  end

module GTChannel : Channel =
  struct
    (* à compléter/modifier *)
    module Queue = Queue
    module GT = GreenThreads

    let create () = 
      (* file de messages *)
      let msgs = Queue.create () in
      let write data = 
        Queue.push data msgs;
        GT.yield ()
      in
      let rec read () =
        if not @@ Queue.is_empty msgs then Queue.pop msgs
        else ( GT.yield (); read ())
      in
      (write, read);;
  end
    
let sieve () =
  let open GreenThreads in
  let rec filter reader =
      let v0 = reader () in
      if v0 = -1 then exit () else Format.printf "%d@." v0;
      yield ();
      let (writer', reader') = GTChannel.create () in
      fork (fun () -> filter reader');
      while true
      do
        let v = reader () in
        yield ();
        if v mod v0 <> 0 then writer' v;
        if v = -1 then exit ()
      done
    in
  let main () =
      let (writer, reader) = GTChannel.create () in
      fork (fun () -> filter reader);
      for i = 2 to 1000
      do
        writer i;
        yield ()
      done;
      writer (-1);
      exit ()
    in
  scheduler main;;
