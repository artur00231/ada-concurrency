with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body vertex is
   
   procedure printInfo(v : in Vertex) is
      
   begin
      Put(Integer(v.ID));
   end printInfo;
   
end vertex;
