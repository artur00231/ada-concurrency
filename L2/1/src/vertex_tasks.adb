with Ada.Numerics.Discrete_Random;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.Task_Lock;
with GNAT.Semaphores; use GNAT.Semaphores;

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

   function simulate(g : in out graph.Graph; k : in Integer; h : in Integer) return Message_Set_Type.Set is
      Min_Delay : Float := 0.200;
      Max_Delay : Float := 0.800;
      Message_Life : Integer := h;

      Messages : Message_Set_Type.Set;
      Destroyed_Messages : Integer := 0;

      task type vertex_loop is
         entry Init(Vertex_Id : IdType; Node_Type : Task_Type);
         entry Recive(Message : in Message_Type);
         entry AbortSend;
         entry Kill;
      end vertex_loop;

      task Watch_Dog is
         entry Update_Vertex_State(Id : IdType; state : Boolean);
         entry Simulation_Ended;
      end Watch_Dog;

      type Mutex is new Binary_Semaphore(True, Default_Ceiling);
      type Network_Map_Array_type is array (IdType range 0 .. IdType(Size(g))) of Integer;
      type Network_Locks_Array_type is array (IdType range 0 .. IdType(Size(g))) of Mutex;
      Network_Map : Network_Map_Array_type := (others => -1);
      Network_Locks : Network_Locks_Array_type;

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
         Now : Ada.Calendar.Time := Ada.Calendar.Clock;
         Epoch : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of(1970, 1, 1, 0.0);

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

         Messages_Send : Integer := 0;
      begin
         Reset(gen, Integer(Ada.Calendar."-"(Now, Epoch)));

         accept Init (Vertex_Id : in IdType; Node_Type : in Task_Type) do
            Vertex_Type := Node_Type;
            Id := Vertex_Id;
         end Init;

         if (Vertex_Type = SOURCE) then
            loop
               select
                  accept Recive (Message : in Message_Type) do
                     Printer.print("pakiet " & Message.Id'Image & " jest w wierzcholku " & Id'Image);
                     The_Message := Message;
                  end Recive;
                  The_Message.Vertices_Id.Append(Integer(Id));
                  g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);
                  The_Message.Life := The_Message.Life - 1;
                  if The_Message.Life = 0 then
                     Printer.print("Zniszczono pakiet: " & The_Message.ID'Image);
                     The_Message.Vertices_Id.Append(-2);

                     GNAT.Task_Lock.Lock;
                     Destroyed_Messages := Destroyed_Messages + 1;
                     Messages.Include(The_Message);
                     GNAT.Task_Lock.Unlock;
                  else

                     --Wait before reciving more messages
                     Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                     delay Duration(Delay_Time);

                     --Send to next node
                     Next_Node := getRandomVertex;

                     Network_Locks(Id).Seize;
                     if Network_Map(Id) /= -2 then
                        Network_Map(Id) := Integer(Next_Node);
                        Network_Locks(Id).Release;
                        Watch_Dog.Update_Vertex_State(Id, True);

                        Send_Loop_SO: loop
                           select
                              Tasks(Next_Node).Recive(The_Message);
                              exit Send_Loop_SO;
                           else
                              delay 1.0;
                              select
                                 accept AbortSend  do
                                    Printer.print("Zniszczono pakiet: " & The_Message.ID'Image & " przez zablokowanie sie sieci");
                                    The_Message.Vertices_Id.Append(-1);
                                    GNAT.Task_Lock.Lock;
                                    Destroyed_Messages := Destroyed_Messages + 1;
                                    Messages.Include(The_Message);
                                    GNAT.Task_Lock.Unlock;
                                 end AbortSend;

                                 exit Send_Loop_SO;
                              else
                                 null;
                              end select;
                           end select;
                        end loop Send_Loop_SO;

                        Watch_Dog.Update_Vertex_State(Id, False);

                        Network_Locks(Id).Seize;
                        Network_Map(Id) := Integer'Min(Integer(-1), Network_Map(Id));
                        Network_Locks(Id).Release;
                     else
                        --Traped
                        Network_Map(Id) := -1;
                        Network_Locks(Id).Release;

                        The_Message.Vertices_Id.Append(-3);
                        GNAT.Task_Lock.Lock;
                        Destroyed_Messages := Destroyed_Messages + 1;
                        Messages.Include(The_Message);
                        GNAT.Task_Lock.Unlock;

                        Printer.print("pakiet " & The_Message.ID'Image & " został złapany w pułapkę");
                     end if;

                     --Wait before reciving more messages
                     Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                     delay Duration(Delay_Time);
                  end if;
               or
                  accept Kill;
                  exit;
               or
                  delay 0.5;
                  if Messages_Send < k then
                     --Wait
                     Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay - 0.5;
                     delay Duration(Delay_Time);

                     --Make new Message
                     The_Message.ID := IdType(Messages_Send);
                     The_Message.Life := Message_Life;
                     Messages_Send := Messages_Send + 1;
                     The_Message.Vertices_Id.Clear;
                     The_Message.Vertices_Id.Append(Integer(Id));
                     g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);

                     --Send to next node
                     Next_Node := getRandomVertex;

                     Printer.print("pakiet " & The_Message.Id'Image & " zostal wyslany");

                     Network_Locks(Id).Seize;
                     if Network_Map(Id) /= -2 then
                        Network_Map(Id) := Integer(Next_Node);
                        Network_Locks(Id).Release;
                        Watch_Dog.Update_Vertex_State(Id, True);

                        Send_Loop_SOS: loop
                           select
                              Tasks(Next_Node).Recive(The_Message);
                              exit Send_Loop_SOS;
                           else
                              delay 1.0;
                              select
                                 accept AbortSend  do
                                    Printer.print("Zniszczono pakiet: " & The_Message.ID'Image & " przez zablokowanie sie sieci");
                                    The_Message.Vertices_Id.Append(-1);
                                    GNAT.Task_Lock.Lock;
                                    Destroyed_Messages := Destroyed_Messages + 1;
                                    Messages.Include(The_Message);
                                    GNAT.Task_Lock.Unlock;
                                 end AbortSend;

                                 exit Send_Loop_SOS;
                              else
                                 null;
                              end select;
                           end select;
                        end loop Send_Loop_SOS;

                        Watch_Dog.Update_Vertex_State(Id, False);

                        Network_Locks(Id).Seize;
                        Network_Map(Id) := Integer'Min(Integer(-1), Network_Map(Id));
                        Network_Locks(Id).Release;
                     else
                        --Traped
                        Network_Map(Id) := -1;
                        Network_Locks(Id).Release;

                        The_Message.Vertices_Id.Append(-3);
                        GNAT.Task_Lock.Lock;
                        Destroyed_Messages := Destroyed_Messages + 1;
                        Messages.Include(The_Message);
                        GNAT.Task_Lock.Unlock;

                        Printer.print("pakiet " & The_Message.ID'Image & " został złapany w pułapkę");
                     end if;

                     --Wait
                     Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                     delay Duration(Delay_Time);
                  end if;
               end select;
            end loop;

         elsif (Vertex_Type = SINK) then
            loop
               select
                  accept Recive (Message : in Message_Type) do
                     Printer.print("pakiet " & Message.Id'Image & " jest w wierzcholku " & Id'Image);
                     The_Message := Message;
                  end Recive;
                  The_Message.Vertices_Id.Append(Integer(Id));
                  g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);

                  if Integer(Random(gen)) rem Integer(g.verteces.Element(Id).Edges.Length + 1) = 0 then
                     Printer.print("pakiet " & The_Message.Id'Image & " zostal odebrany");

                     GNAT.Task_Lock.Lock;
                     Destroyed_Messages := Destroyed_Messages + 1;
                     Messages.Include(The_Message);
                     GNAT.Task_Lock.Unlock;
                  else

                     The_Message.Life := The_Message.Life - 1;
                     if The_Message.Life = 0 then
                        Printer.print("Zniszczono pakiet: " & The_Message.ID'Image);
                        The_Message.Vertices_Id.Append(-2);

                        GNAT.Task_Lock.Lock;
                        Destroyed_Messages := Destroyed_Messages + 1;
                        Messages.Include(The_Message);
                        GNAT.Task_Lock.Unlock;
                     else
                        --Wait before reciving more messages
                        Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                        delay Duration(Delay_Time);

                        --Send to next node
                        Next_Node := getRandomVertex;

                        Network_Locks(Id).Seize;
                        if Network_Map(Id) /= -2 then
                           Network_Map(Id) := Integer(Next_Node);
                           Network_Locks(Id).Release;
                           Watch_Dog.Update_Vertex_State(Id, True);

                           Send_Loop_SI: loop
                              select
                                 Tasks(Next_Node).Recive(The_Message);
                                 exit Send_Loop_SI;
                              else
                                 delay 1.0;
                                 select
                                    accept AbortSend  do
                                       Printer.print("Zniszczono pakiet: " & The_Message.ID'Image & " przez zablokowanie sie sieci");
                                       The_Message.Vertices_Id.Append(-1);
                                       GNAT.Task_Lock.Lock;
                                       Destroyed_Messages := Destroyed_Messages + 1;
                                       Messages.Include(The_Message);
                                       GNAT.Task_Lock.Unlock;
                                    end AbortSend;

                                    exit Send_Loop_SI;
                                 else
                                    null;
                                 end select;
                              end select;
                           end loop Send_Loop_SI;

                           Watch_Dog.Update_Vertex_State(Id, False);

                           Network_Locks(Id).Seize;
                           Network_Map(Id) := Integer'Min(Integer(-1), Network_Map(Id));
                           Network_Locks(Id).Release;
                        else
                           --Traped
                           Network_Map(Id) := -1;
                           Network_Locks(Id).Release;

                           The_Message.Vertices_Id.Append(-3);
                           GNAT.Task_Lock.Lock;
                           Destroyed_Messages := Destroyed_Messages + 1;
                           Messages.Include(The_Message);
                           GNAT.Task_Lock.Unlock;

                           Printer.print("pakiet " & The_Message.ID'Image & " został złapany w pułapkę");
                        end if;
                        --Wait before reciving more messages
                        Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                        delay Duration(Delay_Time);
                     end if;
                  end if;

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
                  The_Message.Vertices_Id.Append(Integer(Id));
                  g.verteces.Reference(Id).Element.Messages.Append(The_Message.Id);
                  The_Message.Life := The_Message.Life - 1;
                  if The_Message.Life = 0 then
                     Printer.print("Zniszczono pakiet: " & The_Message.ID'Image);
                     The_Message.Vertices_Id.Append(-2);

                     GNAT.Task_Lock.Lock;
                     Destroyed_Messages := Destroyed_Messages + 1;
                     Messages.Include(The_Message);
                     GNAT.Task_Lock.Unlock;
                  else

                     --Wait before reciving more messages
                     Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                     delay Duration(Delay_Time);

                     --Send to next node
                     Next_Node := getRandomVertex;

                     Network_Locks(Id).Seize;
                     if Network_Map(Id) /= -2 then
                        Network_Map(Id) := Integer(Next_Node);
                        Network_Locks(Id).Release;
                        Watch_Dog.Update_Vertex_State(Id, True);

                        Send_Loop_N: loop
                           select
                              Tasks(Next_Node).Recive(The_Message);
                              exit Send_Loop_N;
                           else
                              delay 1.0;
                              select
                                 accept AbortSend  do
                                    Printer.print("Zniszczono pakiet: " & The_Message.ID'Image & " przez zablokowanie sie sieci");
                                    The_Message.Vertices_Id.Append(-1);
                                    GNAT.Task_Lock.Lock;
                                    Destroyed_Messages := Destroyed_Messages + 1;
                                    Messages.Include(The_Message);
                                    GNAT.Task_Lock.Unlock;
                                 end AbortSend;

                                 exit Send_Loop_N;
                              else
                                 null;
                              end select;
                           end select;
                        end loop Send_Loop_N;

                        Watch_Dog.Update_Vertex_State(Id, False);

                        Network_Locks(Id).Seize;
                        Network_Map(Id) := Integer'Min(Integer(-1), Network_Map(Id));
                        Network_Locks(Id).Release;
                     else
                        --Traped
                        Network_Map(Id) := -1;
                        Network_Locks(Id).Release;

                        The_Message.Vertices_Id.Append(-3);
                        GNAT.Task_Lock.Lock;
                        Destroyed_Messages := Destroyed_Messages + 1;
                        Messages.Include(The_Message);
                        GNAT.Task_Lock.Unlock;

                        Printer.print("pakiet " & The_Message.ID'Image & " został złapany w pułapkę");
                     end if;

                     --Wait before reciving more messages
                     Delay_Time := (Float(Random(gen)) / Float(IdType'Last)) * (Max_Delay - Min_Delay) + Min_Delay;
                     delay Duration(Delay_Time);
                  end if;
               or
                  accept Kill;
                  exit;
               end select;
            end loop;
         end if;
      end vertex_loop;

      task body Watch_Dog is
         type Nodes_Times_Array_Type is array (IdType range 0 .. IdType(Size(g))) of Time;
         type Nodes_States_Array_Type is array (IdType range 0 ..IdType(Size(g))) of Boolean;
         Nodes_Last_Time : Nodes_Times_Array_Type := (others => Clock);
         Nodes_States : Nodes_States_Array_Type := (others => False);
         Last_Update : Time := Clock;

         Ended : Boolean := False;
         End_Time : Time := Clock;

         function Is_Reciver(id : IdType) return Boolean is
         begin
            for I in 0 .. IdType(Size(g)) loop
               if Network_Map(I) = Integer(id) then
                  return True;
               end if;
            end loop;

            return False;
         end Is_Reciver;
      begin
         Main_Cycle: loop
            select
               accept Update_Vertex_State(Id : IdType; state : Boolean) do
                  Nodes_Last_Time(Id) := Clock;
                  Last_Update := Clock;
                  Nodes_States(Id) := state;
               end Update_Vertex_State;
            else
               delay 1.0;
               if (Clock - Last_Update) > Seconds(5) then
                  for I in reverse 0 .. IdType(Size(g)) loop
                     if Nodes_States(I) = True and then (Clock - Nodes_Last_Time(I)) > Seconds(5) and then
                       Is_Reciver(I) then
                        Printer.print("Deadlock detected");
                        Printer.print("On: " & I'Image);

                        Nodes_Last_Time := (others => Clock);
                        Last_Update := Clock;
                        Tasks(I).AbortSend;
                     end if;
                  end loop;
               end if;

               if k - Destroyed_Messages <= 0 then
                  --Watch_Dog have to wait for other tasks, so they can to call Update_Vertex_State
                  if not Ended then
                     Ended := True;
                     End_Time := Clock;
                  elsif (Clock - End_Time) > Seconds(5) then
                     exit Main_Cycle;
                  end if;
               end if;
            end select;

         end loop Main_Cycle;

         accept Simulation_Ended;
      end Watch_Dog;

   begin
      --Start simulation
      Tasks(IdType(0)).Init(IdType(0), SOURCE);
      for I in IdType(1) .. IdType(size(g) - 2) loop
         Tasks(I).Init(I, NODE);
      end loop;
      Tasks(IdType(size(g) - 1)).Init(IdType(size(g) - 1), SINK);

      Watch_Dog.Simulation_Ended;

      --End simulation
      for I in IdType(0) .. IdType(size(g) - 1) loop
         Tasks(I).Kill;
      end loop;


      Printer.Kill;

      return Messages;
   end simulate;

end Vertex_Tasks;
