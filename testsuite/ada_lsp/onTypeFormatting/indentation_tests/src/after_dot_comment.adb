procedure After_Dot_Comment is
begin
   V
     .  --  Comment
       X := 10;
   V.X := 20;
end After_Dot_Comment;
