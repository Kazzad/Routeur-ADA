with Ada.Strings;               use Ada.Strings;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;  use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line;          use Ada.Command_Line;
with Cache;                     use Cache;

package Tris is

    type T_Adresse_IP is mod 2 ** 32;

    type T_frequence_des_donnees is limited private;

    type T_case_frequence is
	record
            nb_iterations : Integer;
      	    Donnee : T_Donnee;
            Suivant : T_frequence_des_donnees;
	end record;

    procedure Trier_FIFO(Cache : in out T_Cache);

    procedure Trier_LRU(Cache : in out T_Cache);

    procedure Trier_LFU(Cache : in out T_Cache ; Frequence_des_donnees : T_Frequence_des_donnees);

private
        type T_frequence_des_donnees is access T_case_frequence;
    end Tris;
