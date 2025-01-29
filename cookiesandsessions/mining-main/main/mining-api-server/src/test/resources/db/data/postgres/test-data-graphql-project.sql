--TEST CUSTOM PROPERTIES MODULE
UPDATE module SET custom_properties = '{"ModuleCustomProperties":{"customMetaInfo1":"some custom meta 1A value","customMetaInfo2":"some custom meta 2A value"}}'
    WHERE name='PRGA';
UPDATE module SET custom_properties = '{"ModuleCustomProperties":{"customMetaInfo1":"some custom meta 1B value","customMetaInfo2":"some custom meta 2B value"}}'
    WHERE name='PRGB';
