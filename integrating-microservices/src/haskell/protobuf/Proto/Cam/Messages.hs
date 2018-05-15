{- This file was auto-generated from cam/messages.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies,
  UndecidableInstances, MultiParamTypeClasses, FlexibleContexts,
  FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude
  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Proto.Cam.Messages where
import qualified Data.ProtoLens.Reexport.Prelude as Prelude
import qualified Data.ProtoLens.Reexport.Data.Int as Data.Int
import qualified Data.ProtoLens.Reexport.Data.Word as Data.Word
import qualified Data.ProtoLens.Reexport.Data.ProtoLens
       as Data.ProtoLens
import qualified
       Data.ProtoLens.Reexport.Data.ProtoLens.Message.Enum
       as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Reexport.Lens.Family2
       as Lens.Family2
import qualified Data.ProtoLens.Reexport.Lens.Family2.Unchecked
       as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Reexport.Data.Default.Class
       as Data.Default.Class
import qualified Data.ProtoLens.Reexport.Data.Text as Data.Text
import qualified Data.ProtoLens.Reexport.Data.Map as Data.Map
import qualified Data.ProtoLens.Reexport.Data.ByteString
       as Data.ByteString
import qualified Data.ProtoLens.Reexport.Lens.Labels as Lens.Labels

data ScheduleEntry = ScheduleEntry{_ScheduleEntry'entryId ::
                                   !Data.Text.Text,
                                   _ScheduleEntry'datetime :: !Data.Text.Text,
                                   _ScheduleEntry'toFire :: !Data.Text.Text}
                   deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "entryId" f ScheduleEntry ScheduleEntry a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ScheduleEntry'entryId
                 (\ x__ y__ -> x__{_ScheduleEntry'entryId = y__}))
              Prelude.id

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "datetime" f ScheduleEntry ScheduleEntry a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ScheduleEntry'datetime
                 (\ x__ y__ -> x__{_ScheduleEntry'datetime = y__}))
              Prelude.id

instance (a ~ Data.Text.Text, b ~ Data.Text.Text,
          Prelude.Functor f) =>
         Lens.Labels.HasLens "toFire" f ScheduleEntry ScheduleEntry a b
         where
        lensOf _
          = (Prelude..)
              (Lens.Family2.Unchecked.lens _ScheduleEntry'toFire
                 (\ x__ y__ -> x__{_ScheduleEntry'toFire = y__}))
              Prelude.id

instance Data.Default.Class.Default ScheduleEntry where
        def
          = ScheduleEntry{_ScheduleEntry'entryId =
                            Data.ProtoLens.fieldDefault,
                          _ScheduleEntry'datetime = Data.ProtoLens.fieldDefault,
                          _ScheduleEntry'toFire = Data.ProtoLens.fieldDefault}

instance Data.ProtoLens.Message ScheduleEntry where
        descriptor
          = let entryId__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "entry_id"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional entryId)
                      :: Data.ProtoLens.FieldDescriptor ScheduleEntry
                datetime__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "datetime"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional datetime)
                      :: Data.ProtoLens.FieldDescriptor ScheduleEntry
                toFire__field_descriptor
                  = Data.ProtoLens.FieldDescriptor "to_fire"
                      (Data.ProtoLens.StringField ::
                         Data.ProtoLens.FieldTypeDescriptor Data.Text.Text)
                      (Data.ProtoLens.PlainField Data.ProtoLens.Optional toFire)
                      :: Data.ProtoLens.FieldDescriptor ScheduleEntry
              in
              Data.ProtoLens.MessageDescriptor (Data.Text.pack "ScheduleEntry")
                (Data.Map.fromList
                   [(Data.ProtoLens.Tag 1, entryId__field_descriptor),
                    (Data.ProtoLens.Tag 2, datetime__field_descriptor),
                    (Data.ProtoLens.Tag 10, toFire__field_descriptor)])
                (Data.Map.fromList
                   [("entry_id", entryId__field_descriptor),
                    ("datetime", datetime__field_descriptor),
                    ("to_fire", toFire__field_descriptor)])

datetime ::
         forall f s t a b . (Lens.Labels.HasLens "datetime" f s t a b) =>
           Lens.Family2.LensLike f s t a b
datetime
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "datetime")

entryId ::
        forall f s t a b . (Lens.Labels.HasLens "entryId" f s t a b) =>
          Lens.Family2.LensLike f s t a b
entryId
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "entryId")

toFire ::
       forall f s t a b . (Lens.Labels.HasLens "toFire" f s t a b) =>
         Lens.Family2.LensLike f s t a b
toFire
  = Lens.Labels.lensOf
      ((Lens.Labels.proxy#) :: (Lens.Labels.Proxy#) "toFire")