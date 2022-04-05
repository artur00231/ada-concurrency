with Ada.Numerics.Discrete_Random;
with Ada.Calendar; use Ada.Calendar;
with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;

with vertex; use vertex;

package body GraphInit is

   function initGraph(n, d : in Integer) return graph.Graph_Type is
      subtype RandomRange is IdType range 0 .. IdType(n - 1);
      subtype HostRandomRange is IdType range 0 .. IdType(n - 2);
      subtype RandomIntRange is Integer range 0 .. Integer'Last;
      package Rand is new Ada.Numerics.Discrete_Random(RandomRange); 
      package HostRand is new Ada.Numerics.Discrete_Random(HostRandomRange); 
      package RandomInt is new Ada.Numerics.Discrete_Random(RandomIntRange); 
      gen : Rand.Generator;
      HostGen : HostRand.Generator;
      IntGen : RandomInt.Generator;
      
      g : graph.Graph_Type;
      
      tmp : IdType;
      success : Boolean;
      
      from : IdType;
      to : IdType;
      added : Integer;
      
      Now : Time := Clock;
      Epoch : constant Time := Formatting.Time_Of(1970, 1, 1, 0.0);
      
      Next_Hop : Hop_Type;
      
      Num_Of_Hosts : IdType;
      Address : Address_Type := (0, 0);
      Host : Host_Data_Type := (Reciver => Address);
   begin
      added := 0;
      Rand.Reset(gen, Integer(Now - Epoch));
      HostRand.Reset(HostGen, Integer(Now - Epoch));
      RandomInt.Reset(IntGen, Integer(Now - Epoch));
      
      g := createGraph;
      
      for I in 0 .. n - 1 loop
         tmp := addVertex(g);
      end loop;
      
      for I in 0 .. n - 2 loop
         success := addEdge(g, IdType(I), IdType(I + 1), 1);
         success := addEdge(g, IdType(I + 1), IdType(I), 1);
      end loop;
      
      while added /= d loop
         from := Rand.Random(gen);
         to := Rand.Random(gen);
         
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
      
      for I in 0 .. IdType(n - 1) loop
         --Num_Of_Hosts < Size(g)
         Num_Of_Hosts := HostRand.Random(HostGen) / 2 + 1;
         
         pragma Assert (Num_Of_Hosts < IdType(Size(g)));
         
         for J in 0 .. Num_Of_Hosts loop
            g.verteces.Reference(I).Hosts.Append(Host);
         end loop;
      end loop;
      
      for I in 0 .. IdType(n - 1) loop
         Num_Of_Hosts := IdType(g.verteces.Element(I).Hosts.Length);
         
         for J in 0 .. IdType(Num_Of_Hosts - 1) loop
            to := HostRand.Random(HostGen);
            if to >= I then
               to := to + 1;
            end if;
            
            from := IdType(RandomInt.Random(IntGen) rem Integer(g.verteces.Element(to).Hosts.Length));
            
            g.verteces.Reference(I).Hosts.Reference(J).Reciver.Node := to;
            g.verteces.Reference(I).Hosts.Reference(J).Reciver.Id := from;
         end loop;
      end loop;
      
      return g;
      
   end initGraph;

end GraphInit;
