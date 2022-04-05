with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

package body GraphInit is

   function initGraph(n, d, b : in Integer) return graph.Graph is
      subtype RandomRange is IdType range 0 .. IdType(n - 1);
      package Rand is new Ada.Numerics.Discrete_Random(RandomRange); 
      use Rand;
      gen : Generator;
      
      g : graph.Graph;
      
      tmp : IdType;
      success : Boolean;
      
      from : IdType;
      to : IdType;
      added : Integer;
      
      Now : Time := Clock;
      Epoch : constant Time := Formatting.Time_Of(1970, 1, 1, 0.0);
   begin
      added := 0;
      Reset(gen, Integer(Now - Epoch));
      
      g := createGraph;
      
      for I in 0 .. n - 1 loop
         tmp := addVertex(g);
      end loop;
      
      for I in 0 .. n - 2 loop
         success := addEdge(g, IdType(I), IdType(I + 1));
      end loop;
      
      while added /= d loop
         from := Random(gen);
         to := Random(gen);
         
         if from > to then
            tmp := from;
            from := to;
            to := tmp;
         end if;
         
         if addEdge(g, from, to) then
            added := added + 1;
         end if;
         
      end loop;

      added := 0;
      while added /= b loop
         from := Random(gen);
         to := Random(gen);
         
         if from < to then
            tmp := from;
            from := to;
            to := tmp;
         end if;
         
         if addEdge(g, from, to) then
            added := added + 1;
         end if;
         
      end loop;
      
      return g;
      
   end initGraph;

end GraphInit;
