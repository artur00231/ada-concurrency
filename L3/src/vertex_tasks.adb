with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with GraphInit; use GraphInit;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with graphTypes; use graphTypes;
with Ada.Containers.Vectors; use Ada.Containers;

with vertex; use vertex.EdgesMap;


package body Vertex_Tasks is

   task body Printer is
   begin
      loop
         select
            accept print(text : in String) do
               Put_Line(text);
            end print;
         or
            accept Kill;
            exit;
         end select;
      end loop;
   end Printer;

   procedure simulate(g : in out graph.Graph_Type; Shortest_Paths : in Shortest_Paths_Table) is
      Min_Delay : Float := 0.500;
      Max_Delay : Float := 1.500;
      Last_Index : IdType := IdType(Size(g) - 1);

      subtype RandomRange is IdType range 0 .. IdType'Last;
      package Rand is new Ada.Numerics.Discrete_Random(RandomRange);
      use Rand;
      gen : Generator;
      Now : Ada.Calendar.Time := Ada.Calendar.Clock;
      Epoch : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of(1970, 1, 1, 0.0);

      protected type Routing_Table_Type is
         procedure init(Hop_Array : in Hop_Type_Vector.Vector);
         procedure copy(Hop_Array : out Hop_Type_Vector.Vector);

         function getCost(To : in IdType) return Natural;
         procedure updateCost(To, Via : in IdType; Cost : Natural);

         procedure getChanged(To : out IdType; Cost : out Natural);
         function optimal(Id : in IdType) return Boolean;
         private
         Next_Hop : Hop_Type_Vector.Vector;
      end Routing_Table_Type;

      protected End_Counter is
         function get return Integer;
         procedure inc;
      private
         Counter : Integer := 0;
      end End_Counter;
      protected body End_Counter is
         function get return Integer is
         begin
            return Counter;
         end get;
         procedure inc is
         begin
            Counter := Counter + 1;
         end inc;
      end End_Counter;
      protected Random_Float is
         function get return Float;
      end Random_Float;
      protected body Random_Float is
         function get return Float is
         begin
            return (Float(Random(gen)) / Float(IdType'Last));
         end get;
      end Random_Float;

      task type Reciver is
         entry init(g : graph.Graph_Type; Vertex_Id : IdType);

         entry ConnectionCostChanged(From, To : IdType; Cost : Natural);
         entry Kill;
      end Reciver;

      task type Sender is
         entry init(g : graph.Graph_Type; Vertex_Id : IdType);
      end Sender;


      protected body Routing_Table_Type is
         procedure init(Hop_Array : in Hop_Type_Vector.Vector) is
         begin
            Next_Hop := Hop_Array;
         end init;

         procedure copy(Hop_Array : out Hop_Type_Vector.Vector) is
         begin
            Hop_Array := Next_Hop;
         end copy;

         function getCost(To : in IdType) return Natural is
         begin
            return Next_Hop.Element(Natural(To)).Cost;
         end getCost;

         procedure updateCost(To, Via : in IdType; Cost : in Natural) is
         begin
            Next_Hop.Reference(Natural(To)).Cost := Cost;
            Next_Hop.Reference(Natural(To)).Via := Via;
            Next_Hop.Reference(Natural(To)).Changed := True;
         end updateCost;

         procedure getChanged(To : out IdType; Cost : out Natural) is
         begin
            for I in 0 .. Natural(Last_Index) loop
               if Next_Hop.Element(I).Changed then
                  To := IdType(I);
                  Cost := Next_Hop.Element(I).Cost;

                  Next_Hop.Reference(I).Changed := False;

                  return;
               end if;
            end loop;

            To := IdType(Size(g));
         end getChanged;

         function optimal(Id : in IdType) return Boolean is
            All_Optimal : Boolean := True;
         begin
            for I in 0 .. Last_Index loop
               if Next_Hop.Element(Natural(I)).Cost /= Shortest_Paths(Id, I) or else Next_Hop.Element(Natural(I)).Changed then
                  All_Optimal := False;
                  exit;
               end if;
            end loop;

            return All_Optimal;
         end optimal;

      end Routing_Table_Type;

      type Routing_Table_Type_Array is array (IdType range 0 .. Last_Index) of Routing_Table_Type;
      Routing_Tables : Routing_Table_Type_Array;

      type Sender_Array is array (IdType range 0 .. Last_Index) of Sender;
      type Reciver_Array is array (IdType range 0 .. Last_Index) of Reciver;

      Recivers : Reciver_Array;
      Senders : Sender_Array;

      task body Reciver is
         The_Graph : graph.Graph_Type;
         Id : IdType;

         Current_Cost : Natural;
      begin
         accept init (g : graph.Graph_Type; Vertex_Id : IdType) do
            The_Graph := g;
            Id := Vertex_Id;
         end init;

         Reciver_Loop: loop
            select
               accept ConnectionCostChanged (From : in IdType; To : in IdType; Cost : in Natural) do
                  Current_Cost := Routing_Tables(Id).getCost(To);

                  Printer.print("Wieszchołek: " & IdType'Image(Id) & " otrzymal od: " & IdType'Image(From) &
                                  " oferte do " & IdType'Image(To) & " za " & Natural'Image(Cost));

                  if Cost + g.verteces.Element(Id).Edges.Element(From) < Current_Cost then
                     Routing_Tables(Id).updateCost(To, From, Cost + g.verteces.Element(Id).Edges.Element(From));

                     Printer.print("Wieszchołek: " & IdType'Image(Id) & " akceputuje oferte od: " & IdType'Image(From) &
                                     " poprawa z " & Natural'Image(Current_Cost) & " do " & Natural'Image(Cost + g.verteces.Element(Id).Edges.Element(From)));

                  end if;
               end ConnectionCostChanged;
            or
               accept Kill;
               exit Reciver_Loop;
            end select;
         end loop Reciver_Loop;
      end Reciver;

      task body Sender is
         The_Graph : graph.Graph_Type;
         Id : IdType;
         Sleep_Time : Duration;

         To : IdType;
         Cost : Natural;
      begin
         accept init (g : in graph.Graph_Type; Vertex_Id : in IdType) do
            The_Graph := g;
            Id := Vertex_Id;
         end init;

         Sender_Loop: loop
            Sleep_Time := Duration(Random_Float.get * (Max_Delay - Min_Delay) + Min_Delay);
            delay Sleep_Time;

            Routing_Tables(Id).getChanged(To, Cost);

            if To /= Last_Index + 1 then
               for Edge in g.verteces.Reference(Id).edges.Iterate loop
                  Recivers(Key(Edge)).ConnectionCostChanged(Id, To, Cost);

                  Printer.print("Wieszchołek: " & IdType'Image(Id) & " wysyła do: " & IdType'Image(Key(Edge)) &
                                  " oferte do " & IdType'Image(To) & " za " & Natural'Image(Cost));
               end loop;
            else
               if Routing_Tables(Id).optimal(Id) then
                  Printer.print("Wieszchołek: " & IdType'Image(Id) & " konczy prace, sciezki zoptymalizowane");
                  End_Counter.inc;
                  exit Sender_Loop;
               end if;
            end if;

         end loop Sender_Loop;
      end Sender;
   begin
      Reset(gen, Integer(Ada.Calendar."-"(Now, Epoch)));

      for I in 0 .. Last_Index loop
         Routing_Tables(I).init(g.verteces.Reference(I).Next_Hop);
      end loop;

      for I in 0 .. Last_Index loop
         Recivers(I).init(g, I);
      end loop;

      for I in 0 .. Last_Index loop
         Senders(I).init(g, I);
      end loop;

      while End_Counter.get /= Integer(Size(g)) loop
         delay 1.0;
      end loop;

      for I in 0 .. Last_Index loop
         Recivers(I).Kill;
         Routing_Tables(I).copy(g.verteces.Reference(I).Next_Hop);
      end loop;

      Printer.Kill;

   end simulate;

end Vertex_Tasks;
