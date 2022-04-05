with Ada.Containers.Hashed_Maps; use Ada.Containers;

package graphTypes is

   type IdType is new Integer range 0 .. Integer'Last;
   
   function IdHash(i: IdType) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(i));
   
   type Task_Type is (SOURCE, SINK, NODE);

end graphTypes;
