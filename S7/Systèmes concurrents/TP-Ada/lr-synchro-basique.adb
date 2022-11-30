-- Time-stamp: <19 oct 2012 15:00 queinnec@enseeiht.fr>

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

-- Version avec machine à etats. Pas de priorité définie.
package body LR.Synchro.Basique is
   
   function Nom_Strategie return String is
   begin
      return "Basique, par machine à états";
   end Nom_Strategie;
   
   task LectRedTask is
      entry Demander_Lecture;
      entry Demander_Ecriture;
      entry Terminer_Lecture;
      entry Terminer_Ecriture;
   end LectRedTask;

   task body LectRedTask is
   -- Etape 1
      --type EtatFichier is (Libre,Ecriture,Lecture);
      --etat : EtatFichier := Libre;
      EcritureEnCours: Boolean := False;
      nl : integer := 0;
   begin
      loop
         -- Etape 1
         --  case etat is
         --     when Libre =>
         --     select
         --        accept Demander_Lecture do 
         --           etat := Lecture;
         --           nl := nl + 1;
         --        end Demander_Lecture;
         --        or
         --        accept Demander_Ecriture do 
         --           etat := Ecriture;
         --        end Demander_Ecriture;
         --     end select;
            
         --     when Lecture => 
         --     select
         --        accept Demander_Lecture do 
         --           nl := nl + 1;
         --        end Demander_Lecture;
         --     or  
         --        accept Terminer_Lecture do
         --           nl := nl - 1;
         --           if (nl = 0) then
         --              etat := Libre;
         --           end if;
         --        end Terminer_Lecture; 
         --     end select;

         --     when Ecriture =>
         --        accept Terminer_Ecriture do 
         --           etat := Libre;
         --        end Terminer_Ecriture;
         --  end case;  

         -- Etape 2 avec boolean et copilot       
      end loop;
   exception
      when Error: others =>
         Put_Line("**** LectRedTask: exception: " & Ada.Exceptions.Exception_Information(Error));
   end LectRedTask;

   procedure Demander_Lecture is
   begin
      LectRedTask.Demander_Lecture;
   end Demander_Lecture;

   procedure Demander_Ecriture is
   begin
      LectRedTask.Demander_Ecriture;
   end Demander_Ecriture;

   procedure Terminer_Lecture is
   begin
      LectRedTask.Terminer_Lecture;
   end Terminer_Lecture;

   procedure Terminer_Ecriture is
   begin
      LectRedTask.Terminer_Ecriture;
   end Terminer_Ecriture;

end LR.Synchro.Basique;
