with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Ada.Containers.Vectors; use Ada.Containers;
with graphTypes; use graphTypes;

package vertex is
   
   package EdgesMap is new Ada.Containers.Hashed_Maps
     (
      Key_Type => IdType,
      Element_Type => Natural,
      Hash => IdHash,
      Equivalent_Keys => "="
     );

   package Id_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => IdType, "=" => "=");
   
   type Hop_Type is record
      Via : IdType;
      Changed : Boolean;
      Cost : Natural;
   end record;
   
   package Hop_Type_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Hop_Type, "=" => "=");

   type Vertex_Type is record
      ID : IdType;
      Edges : EdgesMap.Map;
      
      Next_Hop : Hop_Type_Vector.Vector;
   end record;
   
   
   function "=" (left, right : Vertex_Type) return Boolean is
        (left.ID = right.ID);
      
   procedure printInfo(v : in Vertex_Type);
 
end vertex;
