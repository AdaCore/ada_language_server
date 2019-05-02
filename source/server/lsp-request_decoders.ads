with Ada.Streams;
with LSP.Messages.Requests; use LSP.Messages.Requests;
package LSP.Request_Decoders is

   procedure Write
     (S : access Ada.Streams.Root_Stream_Type'Class;
      V : Initialize_Request);
   for Initialize_Request'Write use Write;
   
end LSP.Request_Decoders;
