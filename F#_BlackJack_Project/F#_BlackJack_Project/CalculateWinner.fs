namespace BlackjackProject

module CalculateWinner = 
    open Definitions

    module CalculateWinner = 
        let winner (players:Player[]) dealer =
            let listOfHandValues = [] // empty list
            let rec getListOfHandValues (players:Player[]) listOfHandValues num = // recursive function
                if num <= Array.length players - 1 then // if counter is less than or equal to length of players
                    let listOfHandValues = players.[num].HandValue :: listOfHandValues // add handvalue to list
                    let num = num + 1 // add 1 to counter
                    getListOfHandValues players listOfHandValues num // call recursive function

                else
                    listOfHandValues

            let list = getListOfHandValues players listOfHandValues 0 // fill list of handvalues

            let handValues = List.rev list // reverse list order
            let playersList = Array.toList players // turn players into list

            let newList = List.zip handValues playersList // combine lists
            let newList = List.sortBy (fst) newList // sort list
            let finalList = List.map (snd) newList // create new list in order

            let winner, losers = // gets winner from list
                match finalList with
                | winner :: losers -> winner, losers
                |_ -> failwith "Needs at least 1 player"

            if winner.HandValue > dealer.HandValue then // compares winner handvalue to dealer handvalue
                printfn "Player %i wins!" winner.Id
                printfn "Winner hand value: %i" winner.HandValue

            else
                printfn "Dealer wins!"
                printfn "Dealer hand value: %i" dealer.HandValue

