namespace BlackjackProject

module DealerTurn = 
    open Definitions
    open GetHandValue
    open DrawCard

    module DealerTurn = 
        let rec dealerTurn dealer deck = // recursive function
            if dealer.HandValue < 17 // if handvalue is less than 17 then draw a card and update dealer
            then
                let card, deck = Draw.drawCard deck
                let newDealer = {dealer with Hand = Array.append dealer.Hand [|card|]; HandValue = dealer.HandValue + GetCardValue.getCardValue card}
                dealerTurn newDealer deck

            elif dealer.HandValue > 21 then // if handvalue is above 21 then set dealer to bust
                let newDealer = {dealer with State = State.Bust}
                newDealer, deck

            else
                dealer, deck
