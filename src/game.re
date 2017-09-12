type player =
  | Red
  | Blue;

type winner =
  | Draw
  | Player player;

type worldState =
  | Running
  | End winner;

type cell =
  | Empty
  | Marked player;

let lines = [
  [0, 1, 2],
  [3, 4, 5],
  [6, 7, 8],
  [0, 3, 6],
  [1, 4, 7],
  [2, 5, 8],
  [0, 4, 8],
  [2, 4, 6]
];

module type Board = {
  type t;
  let getCell: t => int => cell;
  let setCell: t => int => cell => t;
  let getState: t => worldState;
};

/* TODO exercise by switching to non-mutable implementation */
module Board = {
  type t = array cell;
  let make () => Array.make 9 Empty;
  let getCell board i => board.(i);
  let setCell board i cell => {
    board.(i) = cell;
    board
  };
  let playerWinBoardLine player board line =>
    List.for_all (fun i => getCell board i == Marked player) line;
  let playerWinBoard player board => List.exists (playerWinBoardLine player board) lines;
  let boardIsFull board =>
    not @@
    List.exists
      (
        fun cell =>
          switch cell {
          | Empty => true
          | _ => false
          }
      )
      (Array.to_list board);
  let getState board =>
    if (playerWinBoard Red board) {
      End (Player Red)
    } else if (playerWinBoard Blue board) {
      End (Player Blue)
    } else if (
      boardIsFull board
    ) {
      End Draw
    } else {
      Running
    };
};

module World = {
  type t = {
    board: Board.t,
    player
  };
  let make () => {board: Board.make (), player: Red};
};

let rem x y => Int64.rem (Int64.of_int x) (Int64.of_int y) |> Int64.to_int;

module Cell = {
  let component = ReasonReact.statelessComponent "Cell";
  let style state =>
    ReactDOMRe.Style.make
      width::"50px"
      height::"50px"
      border::"1px solid black"
      backgroundColor::(
        switch state {
        | Empty => "white"
        | Marked Red => "red"
        | Marked Blue => "blue"
        }
      )
      ();
  let make ::onClick ::state _children => {
    ...component,
    render: fun _self => <div onClick style=(style state) />
  };
};

module Row = {
  let style = ReactDOMRe.Style.make display::"flex" flex::"1" ();
  let component = ReasonReact.statelessComponent "Row";
  let make _children => {
    ...component,
    render: fun _self => <div style> (ReasonReact.arrayToElement _children) </div>
  };
};

let style = ReactDOMRe.Style.make display::"flex" flexDirection::"column" height::"150px" ();

type action =
  | Reset
  | Click int;

let component = ReasonReact.reducerComponent "Problem10";

let make _children => {
  ...component,
  reducer: fun action {board, player} =>
    switch action {
    | Click i =>
      switch (Board.getState board) {
      | End _ => ReasonReact.NoUpdate
      | Running =>
        switch (Board.getCell board i) {
        | Empty =>
          let board = Board.setCell board i (Marked player);
          ReasonReact.Update {
            board,
            player:
              switch player {
              | Red => Blue
              | Blue => Red
              }
          }
        | _ => ReasonReact.NoUpdate
        }
      }
    | Reset => ReasonReact.Update {board: Board.make (), player: Red}
    },
  initialState: World.make,
  render: fun {state, reduce} => {
    let state: World.t = state;
    let onClick i _event => Click i;
    <div style=(ReactDOMRe.Style.make width::"150px" ())>
      <div style=(ReactDOMRe.Style.make display::"flex" justifyContent::"space-between" ())>
        <div>
          (
            ReasonReact.stringToElement (
              switch (Board.getState state.board) {
              | Running => "Running..."
              | End winner =>
                switch winner {
                | Player Red => "Red wins!"
                | Player Blue => "Blue wins!"
                | Draw => "Draw!"
                }
              }
            )
          )
        </div>
        <button
          style=(ReactDOMRe.Style.make background::"#eee" border::"1px solid black" ())
          onClick=(reduce (fun _ => Reset))>
          (ReasonReact.stringToElement "Reset")
        </button>
      </div>
      <div style>
        <Row key="0">
          <Cell key="0" onClick=(reduce @@ onClick 0) state=(Board.getCell state.board 0) />
          <Cell key="1" onClick=(reduce @@ onClick 1) state=(Board.getCell state.board 1) />
          <Cell key="2" onClick=(reduce @@ onClick 2) state=(Board.getCell state.board 2) />
        </Row>
        <Row key="1">
          <Cell key="0" onClick=(reduce @@ onClick 3) state=(Board.getCell state.board 3) />
          <Cell key="1" onClick=(reduce @@ onClick 4) state=(Board.getCell state.board 4) />
          <Cell key="2" onClick=(reduce @@ onClick 5) state=(Board.getCell state.board 5) />
        </Row>
        <Row key="2">
          <Cell key="0" onClick=(reduce @@ onClick 6) state=(Board.getCell state.board 6) />
          <Cell key="1" onClick=(reduce @@ onClick 7) state=(Board.getCell state.board 7) />
          <Cell key="2" onClick=(reduce @@ onClick 8) state=(Board.getCell state.board 8) />
        </Row>
      </div>
    </div>
  }
};
