module App.SavedMsg (SavedMsg (..)) where

data SavedMsg = SavedMsg
  { dbId           ∷ Int
  , msgId          ∷ Text
  , opId           ∷ Text
  , interlocutorId ∷ Text
  , content        ∷ Text
  }
