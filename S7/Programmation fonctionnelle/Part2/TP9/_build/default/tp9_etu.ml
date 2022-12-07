
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
  
    let rec scheduler proc_init : unit = match proc_init with
      | [] -> ()
      | p::ps -> match Delimcc.push_prompt promt (fun () -> p()) with
                | Exit -> 
                | Done -> scheduler ps
                | Yield k -> scheduler (ps @ [k]) ;;
    
    let ping () : unit =
      begin
        for i = 1 to 10 do
          Delimcc.print_end_line "ping";
          Delimcc.shift p (fun k -> Yield k); (* rendre la main après chaque affichage *)
        done
      end ;;
    
    let pong () : unit =
      begin
        for i = 1 to 10 do
          Delimcc.print_end_line "pong";
          Delimcc.shift p (fun k -> Yield k); (* rendre la main après chaque affichage *)
        done
      end ;;

    let ping_pong () : unit =
      begin
        ping ();
        pong ();
        Exit (* fin de l'éxécution *)
      end ;;

    let main () = scheduler(ping_pong);;

  end

module type Channel =
  sig
    val create : unit -> ('a -> unit) * (unit -> 'a)
  end

module GTChannel : Channel =
  struct
    (* à compléter/modifier *)
    let create () = assert false;;
  end
    
let sieve () =
  let rec filter reader =
    GreenThreads.(
      let v0 = reader () in
      if v0 = -1 then exit () else
      Format.printf "%d@." v0;
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
    ) in
  let main () =
    GreenThreads.(
      let (writer, reader) = GTChannel.create () in
      fork (fun () -> filter reader);
      for i = 2 to 1000
      do
        writer i;
        yield ()
      done;
      writer (-1);
      exit ()
    ) in
  GreenThreads.scheduler main;;
