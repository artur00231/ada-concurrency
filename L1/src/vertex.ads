with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Ada.Containers.Vectors; use Ada.Containers;
with graphTypes; use graphTypes;

package vertex is
   
   package EdgesSet is new Ada.Containers.Hashed_Sets
     (
      Element_Type => IdType,
      Hash => IdHash,
      Equivalent_Elements => "="
     );
   package Id_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => IdType, "=" => "=");

   type Vertex is record
      ID : IdType;
      Edges : EdgesSet.Set;
      Messages: Id_Vector.Vector;
   end record;
   
   
   function "=" (left, right : Vertex) return Boolean is
        (left.ID = right.ID);
      
   type Message_Type is record
      ID : IdType;
      Vertices_Id : Id_Vector.Vector;
   end record;
   
   procedure printInfo(v : in Vertex);
 
end vertex;
