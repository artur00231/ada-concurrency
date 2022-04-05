with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with GraphInit; use GraphInit;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with graphTypes; use graphTypes;
with Ada.Containers.Vectors; use Ada.Containers;


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

   function simulate(g : in out graph.Graph; k : Integer) return Message_Set_Type.Set is
      Min_Delay : Float := 0.250;
      Max_Delay : Float := 0.800;

      Messages : Message_Set_Type.Set;

      task type vertex_loop is
         entry Init(Vertex_Id : IdType; Node_Type : Task_Type);
         entry Recive(Message : in Message_Type);
         entry Kill;
      end vertex_loop;

      type vertex_loop_access is access vertex_loop;
      type Tasks_Type is array (IdType range <>) of vertex_loop;

      Tasks : Tasks_Type(0 .. IdType(size(g) - 1));

      task body vertex_loop is
         Vertex_Type : Task_Type;
         Id : IdType;

         subtype RandomRange is IdType range 0 .. IdType'Last;
         package Rand is new Ada.Numerics.Discrete_Random(RandomRange);
         use Rand;
         gen : Generator;
         Now : Time := Clock;
         Epoch : constant Time := Formatting.Time_Of(1970, 1, 1, 0.0);

         The_Message : Message_Type;
         Next_Node : IdType;

         Delay_Time : Float;

         function getRandomVertex return IdType is
            Distance : Integer;
            Edges : EdgesSet.Set;
            Last_Elem : IdType;
         begin
            Distance := Integer(Random(gen)) rem Integer(g.verteces.Element(Id).Edges.Length);
            Edges := g.verteces.Element(Id).Edges;
            for E of Edges loop
               Last_Elem := E;
               Distance := Distance - 1;

               if Distance = 0 then
                  exit;
               end if;
            end loop;

            return Last_Elem;
         end getRandomVertex;
      begin
         Reset(gen, Integer(Now - Epoch));

         accept Init (Vertex_Id : in IdType; Node_Type : in Task_Type) do
            Vertex_Type := Node_Type;
            Id := Vertex_Id;
         end Init;

         if (Vertex_Type = SOURCE) then
            for I in 0 .. IdType(k) loop
               --Wait
               Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
               delay Duration(Delay_Time);

               --Make new Message
               The_Message.ID := I;
               The_Message.Vertices_Id.Clear;
               The_Message.Vertices_Id.Append(Id);
               g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);

               --Send to next node
               Next_Node := getRandomVertex;

               Printer.print("pakiet " & The_Message.Id'Image & " zostal wyslany");
               Tasks(Next_Node).Recive(The_Message);
            end loop;

            accept Kill;

         elsif (Vertex_Type = SINK) then
            loop
               select
                  accept Recive (Message : in Message_Type) do
                     Printer.print("pakiet " & Message.Id'Image & " zostal odebrany");
                     The_Message := Message;
                  end Recive;
                  The_Message.Vertices_Id.Append(Id);
                  g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);
                  Messages.Include(The_Message);

                  --Wait before reciving more messages
                  Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                  delay Duration(Delay_Time);
               or
                  accept Kill;
                  exit;
               end select;
            end loop;
         else
            loop
               select
                  accept Recive (Message : in Message_Type) do
                     Printer.print("pakiet " & Message.Id'Image & " jest w wierzcholku " & Id'Image);
                     The_Message := Message;
                  end Recive;
                  The_Message.Vertices_Id.Append(Id);
                  g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);

                  --Wait before reciving more messages
                  Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                  delay Duration(Delay_Time);

                  --Send to next node
                  Next_Node := getRandomVertex;
                  Tasks(Next_Node).Recive(The_Message);

                  --Wait before reciving more messages
                  Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                  delay Duration(Delay_Time);
               or
                  accept Kill;
                  exit;
               end select;
            end loop;
         end if;
      end vertex_loop;

   begin
      --Start simulation
      Tasks(IdType(0)).Init(IdType(0), SOURCE);
      for I in IdType(1) .. IdType(size(g) - 2) loop
         Tasks(I).Init(I, NODE);
      end loop;
      Tasks(IdType(size(g) - 1)).Init(IdType(size(g) - 1), SINK);


      --End simulation
      for I in IdType(0) .. IdType(size(g) - 1) loop
         Tasks(I).Kill;
      end loop;

      Printer.Kill;

      return Messages;
   end simulate;

end Vertex_Tasks;
