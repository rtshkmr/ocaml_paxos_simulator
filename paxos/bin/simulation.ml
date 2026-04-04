open Paxos

let () =
  Cli.(
    Command_unix.run
      (Command.group ~summary:"paxos" [ ("run", command_simulate) ]))
