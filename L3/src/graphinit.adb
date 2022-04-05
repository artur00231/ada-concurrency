with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with vertex; use vertex;

package body GraphInit is

   function initGraph(n, d : in Integer) return graph.Graph_Type is
      subtype RandomRange is IdType range 0 .. IdType(n - 1);
      package Rand is new Ada.Numerics.Discrete_Random(RandomRange); 
      use Rand;
      gen : Generator;
      
      g : graph.Graph_Type;
      
      tmp : IdType;
      success : Boolean;
      
      from : IdType;
      to : IdType;
      added : Integer;
      
      Now : Time := Clock;
      Epoch : constant Time := Formatting.Time_Of(1970, 1, 1, 0.0);
      
      Next_Hop : Hop_Type;
   begin
      added := 0;
      Reset(gen, Integer(Now - Epoch));
      
      g := createGraph;
      
      for I in 0 .. n - 1 loop
         tmp := addVertex(g);
      end loop;
      
      for I in 0 .. n - 2 loop
         success := addEdge(g, IdType(I), IdType(I + 1), 1);
         success := addEdge(g, IdType(I + 1), IdType(I), 1);
      end loop;
      
      while added /= d loop
         from := Random(gen);
         to := Random(gen);
         
         if from > to then
            tmp := from;
            from := to;
            to := tmp;
         end if;
         
         if addEdge(g, from, to, 1) then
            success := addEdge(g, to, from, 1);
            added := added + 1;
         end if;
         
      end loop;
      
      for I in 0 .. IdType(n - 1) loop
         for J in 0 .. IdType(n - 1) loop
            Next_Hop.Changed := True;
            if g.verteces.Reference(I).Element.Edges.Contains(J) then
               Next_Hop.Cost := g.verteces.Element(I).Edges.Element(J);
               Next_Hop.Via := J;
            else
               Next_Hop.Cost := Natural(abs(Integer(I) - Integer(J)));
               
               if J > I then
                  Next_Hop.Via := I + 2;
               elsif J < I then
                 Next_Hop.Via := I - 1;
               else
                  Next_Hop.Via := I;
               end if;
            end if;
            
            
            g.verteces.Reference(I).Element.Next_Hop.Append(Next_Hop);
         end loop;
      end loop;
      
      return g;
      
   end initGraph;

end GraphInit;
