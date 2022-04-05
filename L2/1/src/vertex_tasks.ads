with Ada.Containers.Hashed_Sets; use Ada.Containers;
with vertex; use vertex;
with graph; use graph;

package Vertex_Tasks is
   
   task Printer is
      entry print(text : in String);
      entry Kill;
   end Printer;
   
   function MessageHash(m : Message_Type) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type(m.ID));
   
   package Message_Set_Type is new Ada.Containers.Hashed_Sets
     (
      Element_Type => Message_Type,
      Hash => MessageHash,
      Equivalent_Elements => "="
     );
   
   function simulate(g : in out graph.Graph; k : in Integer; h : in Integer) return Message_Set_Type.Set;

end Vertex_Tasks;
