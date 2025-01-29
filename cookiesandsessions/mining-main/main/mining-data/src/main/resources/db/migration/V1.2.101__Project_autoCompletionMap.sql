CREATE PROPERTY Project.autoCompletionMap EMBEDDEDMAP EMBEDDEDLIST;
UPDATE Project SET autoCompletionMap = first(out('HasAdditionalInfo')[@class='ProjectCustomProperties']).autoCompletionMap WHERE out_HasAdditionalInfo.in.@class = 'ProjectCustomProperties';
