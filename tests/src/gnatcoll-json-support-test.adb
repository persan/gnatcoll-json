pragma Ada_2012;
with GNAT.OS_Lib;
with Ada.IO_Exceptions;
package body GNATCOLL.JSON.Support.Test is

   ---------------------
   -- Read_Json_Value --
   ---------------------

   function Read (From_Path : String) return String is
      use GNAT.OS_Lib;
      Fd          : File_Descriptor;
      Length      : Natural;
      Read_Length : Integer;
   begin
      Fd  := Open_Read (From_Path, GNAT.OS_Lib.Binary);
      if Fd = Invalid_FD then
         raise Ada.IO_Exceptions.Name_Error with "unable to open " & From_Path;
      end if;
      Length :=  Natural (File_Length (Fd));
      return Ret : String (1 .. Length) do
         Read_Length := Read (Fd, Ret (Ret'First)'Address, Length);
         Close (Fd);
         if Read_Length /= Length then
            raise Ada.IO_Exceptions.Data_Error with "Unable to read complete file";
         end if;
      end return;
   end Read;

   function Read_Json_Value (From_Path : String) return JSON_Value is
   begin
      return Read (Read (From_Path), From_Path);
   end Read_Json_Value;

end GNATCOLL.JSON.Support.Test;
