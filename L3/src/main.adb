with Vertex_Tasks; use Vertex_Tasks;
with graphTypes; use graphTypes;
with graph; use graph;
with GraphInit; use GraphInit;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with GNAT.Exception_Traces;

procedure Main is
   n : Integer := 20;
   d : Integer := 10;

   g : graph.Graph_Type;
begin

   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);

   if (Argument_Count /= 2) then
      Put_Line("Invalid arguments");
      Printer.Kill;
      return;
   end if;

   n := Integer'Value(Argument(Number => 1));
   d := Integer'Value(Argument(Number => 2));

   declare
      Shortest_Paths : Shortest_Paths_Table(0 .. IdType(n - 1), 0 .. IdType(n - 1));
   begin

      g := initGraph(n, d);

      print(g);
      printf(g);

      for I in 0 .. Shortest_Paths'Last(1) loop
         for J in 0 .. Shortest_Paths'Last(2) loop
            Put(g.verteces.Element(I).Next_Hop.Element(Natural(J)).Cost, 3);
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
      Put_Line("");
      Put_Line("");

      calculateShortestPaths(g, Shortest_Paths);

      for I in 0 .. Shortest_Paths'Last(1) loop
         for J in 0 .. Shortest_Paths'Last(2) loop
            Put(Shortest_Paths(I, J), 3);
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
      Put_Line("");
      Put_Line("");

      simulate(g, Shortest_Paths);

      for I in 0 .. Shortest_Paths'Last(1) loop
         for J in 0 .. Shortest_Paths'Last(2) loop
            Put(g.verteces.Element(J).Next_Hop.Element(Natural(I)).Cost, 3);
         end loop;
         Put_Line("");
      end loop;
      Put_Line("");
      Put_Line("");
      Put_Line("");

   end;
end Main;
