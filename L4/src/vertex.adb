with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body vertex is
   
   procedure printInfo(v : in Vertex_Type) is
      
   begin
      Put(Integer(v.ID));
   end printInfo;
   
   function "<" (left, right : Address_Type) return Boolean is
   begin
      if left.Node = right.Node then
         return left.Id < right.Id;
      end if;
      
      return left.Node < right.Node;
   end "<";
   
end vertex;
