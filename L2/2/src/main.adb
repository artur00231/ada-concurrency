with Vertex_Tasks; use Vertex_Tasks;
with graphTypes; use graphTypes;
with graph; use graph;
with GraphInit; use GraphInit;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   n : Integer := 20;
   d : Integer := 10;
   k : Integer := 10;
   b : Integer := 10;
   h : Integer := 30;

   g : graph.Graph;

   Messages : Message_Set_Type.Set;
begin

   if (Argument_Count /= 5) then
      Put_Line("Invalid arguments");
      Printer.Kill;
      return;
   end if;

   n := Integer'Value(Argument(Number => 1));
   d := Integer'Value(Argument(Number => 2));
   k := Integer'Value(Argument(Number => 3));
   b := Integer'Value(Argument(Number => 4));
   h := Integer'Value(Argument(Number => 5));

   Put_Line("Init");

   g := initGraph(n, d, b);

   Put_Line("Inited");

   print(g);
   printf(g);

   Messages := simulate(g, k, h);

   Put_Line("");
   Put_Line("");
   Put_Line("Wiadomosci");

   for Message of Messages loop
      Put_Line("Wiadomosc " & Message.ID'Image & ": ");

      for ID of Message.Vertices_Id loop
         Put_Line(ASCII.HT & ID'Image);
         if ID = -1 then
            Put_Line(ASCII.HT & "Zniszczono przez zablokowanie sie sieci");
         elsif ID = -2 then
            Put_Line(ASCII.HT & "Zniszczono przez brak zycia");
         elsif ID = -3 then
            Put_Line(ASCII.HT & "Zlapano w polapke");
         end if;

      end loop;
   end loop;

   Put_Line("");
   Put_Line("");
   Put_Line("Wieszcholki");

   for V of g.verteces loop
      Put_Line("Wieszcholek " & v.ID'Image & ": ");

      for ID of v.Messages loop
         Put_Line(ASCII.HT & ID'Image);
      end loop;
   end loop;

end Main;
