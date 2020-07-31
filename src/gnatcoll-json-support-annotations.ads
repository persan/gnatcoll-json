package GNATCOLL.JSON.Support.Annotations is
   type Annotion is (Simple,     -- Use simple JSON form.
                     Field_Name, -- Use as field name in JSON-object.
                     Ignore);    -- Dont generate any JSON- code for this type.
end GNATCOLL.JSON.Support.Annotations;
