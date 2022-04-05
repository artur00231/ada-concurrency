with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Vertex; use Vertex;
with graphTypes; use graphTypes;

package graph is
   
   package VertecesMap is new Ada.Containers.Hashed_Maps
     (
      Key_Type => IdType,
      Element_Type => Vertex.Vertex,
      Hash => IdHash,
      Equivalent_Keys => "="
     );

   type Graph is record
      verteces : VertecesMap.Map;
      Avaiable_name : IdType := 0;
   end record;
   
   function createGraph return Graph;
   
   function addVertex(g : in out Graph) return IdType;
   
   function addEdge(g : in out Graph; from, to : IdType) return Boolean;
   
   function size(g : in Graph) return Count_Type is
     (g.verteces.Length);
   
   function connectionsCount(g : in Graph) return Count_Type;
   
   procedure print(g : in Graph);
   
   procedure printf(g : in Graph);

end graph;
