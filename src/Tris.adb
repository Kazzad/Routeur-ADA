with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Cache;
package body Tris is


    Procedure Trier_Fifo(Cache : in out T_cache) is
	Premiere_case : T_cache;
	begin
		Premiere_case:=Cache;
		while not(Est_Vide(Cache.suivant)) loop
			Cache:=Cache.suivant;
		end loop;
		Cache:=Null;
		free(Cache);
		Cache:=Premiere_case;
    end Trier_Fifo;


    Procedure Trier_LRU(Cache : in out T_Cache) is
        Courant : T_Cache;
        Precedent : T_Cache;
    begin
        Courant := Cache;
        While Courant /= null loop
            Precedent := Courant;
            Courant := Courant.suivant;
        end loop;
        if Precedent /= null then
            Precedent.suivant := null;
        end if;
        courant := null;
        cache := New T_cellule'(Precedent.destination, Precedent.Masque, Precedent.eth, cache);
    end Trier_LRU;

    Procedure Trier_LFU(Cache : in out T_Cache ; frequence_des_donnees : in out T_frequence_des_donnees) is
    donnee_moins_utilisee : T_Donnee;
    Min : Integer;
    begin
    Min := frequence_des_donnees.nb_iterations;
    donnee_moins_utilisee := frequence_des_donnees.donnee;
    for i in 2..taille loop
        if frequence_des_donnees.nb_iterations < Min then
            Min := frequence_des_donnees.all.nb_iterations;
            donnee_moins_utilisee := frequence_des_donnees.donnee;
            frequence_des_donnees := frequence_des_donnees.suivant;
        else
            frequence_des_donnees:=frequence_des_donnees.suivant;
        end if;
    end loop;
    return donnee_moins_utilisee;
    end Trier_LFU;

end Tris ;