with Ada.Containers.Hashed_Sets; use Ada.Containers;
with vertex; use vertex;
with graph; use graph;

package Vertex_Tasks is
   
   task Printer is
      entry print(text : in String);
      entry Kill;
   end Printer;
   
   
   procedure simulate(g : in out graph.Graph_Type; Shortest_Paths : in Shortest_Paths_Table);

end Vertex_Tasks;
