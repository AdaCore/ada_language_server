with Children; use Children;
package Great_Children is

   type Great_Child is new Child with null Record;

   procedure Primitive (Self : Great_Child);

end Great_Children;
