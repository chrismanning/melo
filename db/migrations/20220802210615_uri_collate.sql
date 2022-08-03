-- migrate:up

create collation numeric (provider = icu, locale = 'en@colNumeric=yes');

alter table source alter column source_uri type text collate numeric;

-- migrate:down

alter table source alter column source_uri type text collate "default";

drop collation numeric;
