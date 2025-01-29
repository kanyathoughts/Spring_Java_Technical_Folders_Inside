UPDATE DataDictionaryEntry SET definedLocation='Copybook' WHERE in_HasDataDictionaryEntry[0].out.objectTypeLink.typeLink.name IN ['CONTROLCARD','COPYBOOK','COPYCODE','COPYLIB','COPYPROC','GDA','INCLUDE','INLINE_PROC','LDA','PDA','PROC'];
UPDATE DataDictionaryEntry SET definedLocation='Program' WHERE definedLocation != 'Copybook';
