with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body graph is
   
   function createGraph return Graph is
      g : Graph;
   begin
      
      g.Avaiable_name := 0;
      
      return g;
      
   end createGraph;
   
   
   function addVertex(g : in out Graph) return IdType is
      v : Vertex.Vertex;
   begin
         
      v.ID := g.Avaiable_name;
      g.Avaiable_name := g.Avaiable_name + 1;
      g.verteces.Include(Key => v.ID, New_Item => v);
      
      return v.ID;
      
   end addVertex;
   
   
   function addEdge(g : in out Graph; from, to : IdType) return Boolean is
      procedure update(id : in IdType; v : in out Vertex.Vertex) is
      begin 
         v.Edges.Include(to);
      end update;
      
   begin
      
      if from >= g.Avaiable_name or else to >= g.Avaiable_name then
         return False;
      end if;
      
      if from = to then
         return False;
      end if;
      
      if g.verteces.Element(from).Edges.Contains(to) then
         return False;
      end if;
      
      g.verteces.Update_Element(g.verteces.Find(from), update'Access); 
         
      return True;
      
   end addEdge;
   
   
   function connectionsCount(g : in Graph) return Count_Type is
      Count : Count_Type;
   begin
      
      Count := 0;
      
      for Vertex of g.verteces loop
         
         Count := Count + Vertex.Edges.Length;
         
      end loop;
      
      return Count;
      
   end connectionsCount;
   
   
   procedure print(g : in Graph) is
      graph_size : Count_Type;
      v : vertex.Vertex;     
   begin
      
      Put("#Vertices = ");
      Put(Integer(size(g)), 3);
      Put("; #Edges = ");
      Put(Integer(connectionsCount(g)), 3);
      Put_Line("");
      
      graph_size := size(g);
      
      for I in 0 .. graph_size - 1 loop
         v := g.verteces.Element(IdType(I));
         Put("(");
         Put(Integer(v.ID), 3);
         Put_Line(") =>");
         
         for to of v.Edges loop
            Put(ASCII.HT);
            Put("=> (");
            Put(Integer(to), 3);
            Put_Line(")");
         end loop;
      end loop;
      
   end print;
   
   
   procedure printf(g : in Graph) is
      
      type Level_Type is record
         to : IdType;
         used : Boolean;
         wait : Boolean;
      end record;
      
      
      package LevelVectors is new Ada.Containers.Vectors
        (Index_Type   => Natural,
         Element_Type => Level_Type);

      Levels : LevelVectors.Vector;
      
      procedure draw_edges(Levels : in out LevelVectors.Vector; On_Vertex : Boolean; Vertex_Id : IdType) is
         Line : Unbounded_String := Null_Unbounded_String;
         Clear_Line : Boolean := True;
         Fill_To_Level : Integer := 0;
         Level : Level_Type;
         Next_Free_Spcace : Integer;
         Found : Boolean;
         Connected : Boolean := False;
      begin
                           
         for I in 1 .. 3 * Levels.Length loop
            Line := Line & ' ';
         end loop;
         
         if On_Vertex then
            
            for I in 0 .. Integer(Levels.Length - 1) loop
               Level := Levels.Element(I);
               
               if Level.wait then
                  Level.wait := False;
                  Levels.Replace_Element(I, Level);
               end if;
               
               if Level.used then
                  Replace_Element(Line, 3 * I + 1, '|');
                  Clear_Line := False;
               end if;
               
            end loop;
            
            for I in 0 .. Integer(Levels.Length - 1) loop
               Level := Levels.Element(I);
               
               if Level.used and then Level.to = Vertex_Id then
                  
                  Level.used := False;
                  Level.wait := True;
                  Levels.Replace_Element(I, Level);
                  
                  Replace_Element(Line, 3 * I + 1, '*');
                  Clear_Line := False;
                  Fill_To_Level := (if Fill_To_Level > I then Fill_To_Level else I);
                  
                  if (Level.to + 1 /= Vertex_Id) then
                     Connected := True;
                  end if;
                  
               end if;
            end loop;
            
            for Next_Vertex_Id of g.verteces.Element(Vertex_Id).Edges loop
               if Next_Vertex_Id /= Vertex_Id + 1 then
                  
                  Next_Free_Spcace := 0;
                  Found := False;
                  
                  for I in 0 .. Integer(Levels.Length - 1) loop
                     Level := Levels.Element(I);
                     
                     if not Found then
                        if (not Level.used) and then (not Level.wait) then
                           Next_Free_Spcace := I;
                           Found := True;
                        end if;
                     end if;
                     
                  end loop;
                  
                  if not Found then
                     Next_Free_Spcace := Integer(Levels.Length);
                     Line := Line & "*  ";
                     Clear_Line := False;
                     Fill_To_Level := (if Fill_To_Level > Next_Free_Spcace then Fill_To_Level else Next_Free_Spcace);
                     Levels.Append((Next_Vertex_Id, True, False));
                  else
                     Level := Levels.Element(Next_Free_Spcace);
                     Level.used := True;
                     Level.to := Next_Vertex_Id;
                     Levels.Replace_Element(Next_Free_Spcace, Level);
                     
                     Replace_Element(Line, 3 * Next_Free_Spcace + 1, '*');
                     Fill_To_Level := (if Fill_To_Level > Next_Free_Spcace then Fill_To_Level else Next_Free_Spcace);
                     Clear_Line := False;
                  end if;
                  
                  Connected := True;
               end if; 
            end loop;

            for I in 1 .. (3 * Fill_To_Level) loop
               if Element(Line, I) = ' ' then
                  Replace_Element(Line, I, '-');
               end if;
            end loop;
            
         else
            
            for I in 0 .. Integer(Levels.Length - 1) loop
               
               if Levels.Element(I).used then
                  Replace_Element(Line, 3 * I + 1, '|');
                  Clear_Line := False;
               end if;
               
            end loop;

         end if;
         
         if not Clear_Line then
            if Connected then
               Put("--");
               Put(To_String(Line));
            else
               Put("  ");
               Put(To_String(Line));
            end if;
         end if;
                           
      end draw_edges;
   begin
      
      Put("#Vertices = ");
      Put(Integer(size(g)), 3);
      Put("; #Edges = ");
      Put(Integer(connectionsCount(g)), 3);
      Put_Line("");
      
      for I in 0 .. IdType(size(g) - 2) loop
         Put("(");
         Put(Integer(I), 3);
         Put(") *");
         draw_edges(Levels, True, I);
         Put_Line("");
         
         Put("      |");
         draw_edges(Levels, False, I);
         Put_Line("");
         Put("      |");
         draw_edges(Levels, False, I);
         Put_Line("");
         Put("      |");
         draw_edges(Levels, False, I);
         Put_Line("");
         
      end loop;
      
      Put("(");
      Put(Integer(size(g) - 1), 3);
      Put(") *");
      draw_edges(Levels, True, IdType(size(g) - 1));
      Put_Line("");
      
   end printf;
   

end graph;
