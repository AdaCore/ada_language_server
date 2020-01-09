with Children; use Children;
package Great_Children is

   type Great_Child is new Child with null Record;

   procedure Primitive (Self : in out Great_Child;
                        Id   : Integer);

end Great_Children;
