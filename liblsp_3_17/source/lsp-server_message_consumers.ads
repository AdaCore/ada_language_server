--
--  Copyright (C) 2024, AdaCore
--

--  Consumer interface to process client->server messages created by the
--  Server_Message_Factory.

with LSP.Server_Messages;

package LSP.Server_Message_Consumers
   with Preelaborate
is

   type Server_Message_Consumer is limited interface;

   not overriding procedure On_Message
     (Self    : in out Server_Message_Consumer;
      Message : not null LSP.Server_Messages.Server_Message_Access)
        is abstract;
   --  Process message. Implementation is responsible to release memory
   --  occupied by message.

end LSP.Server_Message_Consumers;
