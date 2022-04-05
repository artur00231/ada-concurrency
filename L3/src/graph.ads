with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Vertex; use Vertex;
with graphTypes; use graphTypes;

package graph is
   
   package VertecesMap is new Ada.Containers.Hashed_Maps
     (
      Key_Type => IdType,
      Element_Type => Vertex.Vertex_Type,
      Hash => IdHash,
      Equivalent_Keys => "="
     );
   
   type Graph_Type is record
      verteces : VertecesMap.Map;
      Avaiable_name : IdType := 0;
      
   end record;
   
   function createGraph return Graph_Type;
   
   function addVertex(g : in out Graph_Type) return IdType;
   
   function addEdge(g : in out Graph_Type; from, to : IdType; Cost : Natural) return Boolean;
   
   function size(g : in Graph_Type) return Count_Type is
     (g.verteces.Length);
   
   function connectionsCount(g : in Graph_Type) return Count_Type;
   
   procedure print(g : in Graph_Type);
   
   procedure printf(g : in Graph_Type);
   
   type Shortest_Paths_Table is array (IdType range<>, IdType range<>) of Natural;
   
   procedure calculateShortestPaths(g : in Graph_Type; Shortest_Paths : out Shortest_Paths_Table) with 
     Pre => Shortest_Paths'First(1) = 0 and Shortest_Paths'Last(1) = IdType(size(g)) and
     Shortest_Paths'First(2) = 0 and Shortest_Paths'Last(2) = IdType(size(g));

end graph;
