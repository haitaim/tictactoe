with Ada.Characters.Handling;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;

package body Tictactoe is

    type Tile_Mark is (X, O, None);

    subtype Board_Row is Character range 'A' .. 'C';

    subtype Board_Column is Character range '1' .. '3';

    type Game_Board is array (Board_Row, Board_Column) of Tile_Mark;

    type Tile_Coord is record
        Row : Board_Row;
        Column : Board_Column;
    end record;

    type Player_Controller is (Human, Computer);

    subtype Player_Tile_Mark is Tile_Mark range X .. O;

    type Player_Info is record
        Controller : Player_Controller;
        Player_Mark : Player_Tile_Mark;
    end record;

    type Player_Index is mod 2;

    type Game_Players is array (Player_Index) of Player_Info;

    package Tile_Generator is

        function Generate_Tile return Tile_Coord;

        procedure Initialize;

    end Tile_Generator;

    package body Tile_Generator is
        
        package Board_Row_Generator is new Ada.Numerics.Discrete_Random(Board_Row);
        use Board_Row_Generator;

        Row_Generator : Generator;

        procedure Initialize is
        begin
            Reset(Row_Generator);
        end Initialize;

        function Generate_Tile return Tile_Coord is
            function Row_To_Column(Row : Board_Row) return Board_Column is
            begin
                case Row is
                    when 'A' => return '1';
                    when 'B' => return '2';
                    when 'C' => return '3';
                end case;
            end Row_To_Column;

            Row : constant Board_Row := Random(Row_Generator);
            Column : constant Board_Column := Row_To_Column(Random(Row_Generator));
        begin
            return (Row, Column);
        end Generate_Tile;

    end Tile_Generator;

    function Tile_Mark_To_Char(Mark : Tile_Mark) return Character is
    begin
        case Mark is
            when X => return 'X';
            when O => return 'O';
            when None => return ' ';
        end case;
    end Tile_Mark_To_Char;

    procedure Print_Board(Board : Game_Board) is
    begin
        New_Line;
        Put_Line("  123");
        for I in Board_Row loop
            Put(I & ' ');
            for J in Board_Column loop
                Put(Tile_Mark_To_Char(Board(I,J)));
            end loop;
            New_Line;
        end loop;
        New_Line;
    end Print_Board;

    function Tile_Mark_To_Player_Number(Mark : Player_Tile_Mark) return Character is
    begin
        case Mark is
            when X => return '1';
            when O => return '2';
        end case;
    end Tile_Mark_To_Player_Number;

    function Select_Player return Game_Players is
        procedure Display_Player_Selection is
        begin
            Put_Line("Select who will play");
            Put_Line("    1. Player vs Player");
            Put_Line("    2. Player vs Computer");
            Put_Line("    3. Computer vs Computer");
        end Display_Player_Selection;
    begin
        Display_Player_Selection;
        loop
            declare
                Input : constant String := Get_Line;
            begin
                if Input'Length = 1 then
                    case Input(Input'First) is
                        when '1' => return ((Human, X), (Human, O));
                        when '2' => return ((Human, X), (Computer, O));
                        when '3' => return ((Computer, X), (Computer, O));
                        when others => Put_Line("Invalid selection");
                    end case;
                else
                    Put_Line("Invalid selection");
                end if;
            end;
        end loop;
    end Select_Player;

    procedure Set_Marker(Player : Player_Info; Board : in out Game_Board) is
        function Human_Select return Tile_Coord is
            procedure Get(Input_Coord : out Tile_Coord) is
                use Ada.Characters.Handling;

                function Is_Valid_Form(Input : String) return Boolean is
                begin
                    if Input'Length = 2 then
                        declare
                            First : constant Character := Input(Input'First);
                            Second : constant Character := Input(Input'First + 1);
                        begin
                            return Is_Letter(First) and Is_Digit(Second);
                        end;
                    else
                        return False;
                    end if;
                end Is_Valid_Form;

                function Is_In_Range(Row : Character; Column : Character) return Boolean is
                    Is_Valid_Row : constant Boolean := (case Row is
                        when Board_Row => True,
                        when others => False);
                    Is_Valid_Column : constant Boolean := (case Column is
                        when Board_Column => True,
                        when others => False);
                begin
                    return Is_Valid_Row and Is_Valid_Column;
                end Is_In_Range;

                Row : Character;
                Column : Character;
            begin
                loop
                    declare
                        Input : constant String := Get_Line;
                    begin
                        if Is_Valid_Form(Input) then
                            Row := To_Upper(Input(Input'First));
                            Column := Input(Input'First + 1);
                            exit when Is_In_Range(Row, Column);
                        end if;
                        Put_Line("Invalid selection");
                    end;
                end loop;
                Input_Coord := (Row, Column);
            end Get;

            Tile : Tile_Coord;
        begin
            Put_Line("Player " & Tile_Mark_To_Player_Number(Player.Player_Mark) & ':');
            Put_Line("Enter your selection");
            loop
                Get(Tile);
                exit when Board(Tile.Row, Tile.Column) = None;
                Put_Line("Invalid move");
            end loop;
            return Tile;
        end Human_Select;

        function Computer_Select return Tile_Coord is
            use Tile_Generator;

            Tile : Tile_Coord;
        begin
            loop
                Tile := Generate_Tile;
                exit when Board(Tile.Row, Tile.Column) = None;
            end loop;
            return Tile;
        end Computer_Select;

        Tile : constant Tile_Coord := (case Player.Controller is
            when Human => Human_Select,
            when Computer => Computer_Select);
    begin
        Board(Tile.Row, Tile.Column) := Player.Player_Mark;
    end Set_Marker;

    function Has_Winner(Board : Game_Board) return Tile_Mark is
        type Mark_Line is array (1 .. 3) of Tile_Mark;

        function Is_Finished(Line : Mark_Line) return Boolean is
        begin
            return Line(1) /= None and Line(1) = Line(2) and Line(1) = Line(3);
        end Is_Finished;

        function Has_Finished_Row return Tile_Mark is
            Row : Mark_Line;
        begin
            for I in Board_Row loop
                Row := (Board(I, '1'), Board(I, '2'), Board(I, '3'));
                if Is_Finished(Row) then
                    return Row(1);
                end if;
            end loop;

            return None;
        end Has_Finished_Row;

        function Has_Finished_Column return Tile_Mark is
            Column : Mark_Line;
        begin
            for J in Board_Column loop
                Column := (Board('A', J), Board('B', J), Board('C', J));
                if Is_Finished(Column) then
                    return Column(1);
                end if;
            end loop;

            return None;
        end Has_Finished_Column;

        function Has_Finished_Diagonal return Tile_Mark is
            Diagonal : Mark_Line;
        begin
            Diagonal := (Board('A', '1'), Board('B', '2'), Board('C', '3'));
            if Is_Finished(Diagonal) then
                return Diagonal(1);
            end if;

            Diagonal := (Board('A', '3'), Board('B', '2'), Board('C', '1'));
            if Is_Finished(Diagonal) then
                return Diagonal(1);
            end if;

            return None;
        end Has_Finished_Diagonal;

        Mark : Tile_Mark;
    begin
        Mark := Has_Finished_Row;
        if Mark /= None then
            return Mark;
        end if;

        Mark := Has_Finished_Column;
        if Mark /= None then
            return Mark;
        end if;

        return Has_Finished_Diagonal;
    end Has_Winner;

    procedure Print_Winner(Winner : Tile_Mark) is
        function Get_Number(Mark : Player_Tile_Mark) return Character
            renames Tile_Mark_To_Player_Number;
    begin
        New_Line;
        case Winner is
            when Player_Tile_Mark => Put_Line("Player " & Get_Number(Winner) & " wins");
            when None => Put_Line("Draw");
        end case;
    end Print_Winner;

    procedure Game is
        Board : Game_Board := (
            'A' => (others => None),
            'B' => (others => None),
            'C' => (others => None));
        Players : constant Game_Players := Select_Player;
        Player_Turn : Player_Index := Player_Index'First;
        Current_Player : Player_Info := Players(Player_Turn);
        Winning_Mark : Tile_Mark := None;
    begin
        Tile_Generator.Initialize;

        for Turns in 1 .. 9 loop
            Print_Board(Board);
            Set_Marker(Current_Player, Board);
            Winning_Mark := Has_Winner(Board);
            exit when Winning_Mark /= None;
            Player_Turn := Player_Index'Succ(Player_Turn);
            Current_Player := Players(Player_Turn);
        end loop;

        Print_Board(Board);
        Print_Winner(Winning_Mark);
    end Game;

    procedure Title is
    begin
        Put_Line("Tic-tac-toe!");
    end Title;

end Tictactoe;

