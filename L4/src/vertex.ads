with Ada.Containers.Hashed_Maps; use Ada.Containers;
with Ada.Containers.Hashed_Sets; use Ada.Containers;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces; use Ada.Containers;
with Ada.Containers.Unbounded_Synchronized_Queues; use Ada.Containers;
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

   type Address_Type is record
      Node : IdType;
      Id : IdType;
   end record;
   
   type Host_Data_Type is record
      Reciver : Address_Type;
   end record;
   
   package Host_Data_Vector is new Ada.Containers.Vectors(Index_Type => IdType, Element_Type => Host_Data_Type, "=" => "=");
   
   type Vertex_Type is record
      ID : IdType;
      Edges : EdgesMap.Map;
      
      Next_Hop : Hop_Type_Vector.Vector;
      Hosts : Host_Data_Vector.Vector;
   end record;
   
   type Data_Message_Type is record
      From : Address_Type;
      To : Address_Type;
      
      Via : Id_Vector.Vector;
   end record;
   
   package Data_Message_Queue_Interfaces_Type is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Data_Message_Type);
   package Data_Message_Queues_Type is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Data_Message_Queue_Interfaces_Type);
   
   function "=" (left, right : Vertex_Type) return Boolean is
     (left.ID = right.ID);
   
   function "<" (left, right : Address_Type) return Boolean;
      
   procedure printInfo(v : in Vertex_Type);
 
end vertex;
