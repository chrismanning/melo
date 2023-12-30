-- migrate:up

alter table genre
    add top_level boolean;

alter table genre_parent
    drop constraint if exists parent_genre_pk;

alter table genre_parent
    add constraint parent_genre_pk
        primary key (genre_id, parent_genre);

create view genre_top_level as
with genre_top_level as (select *
                         from genre
                         where top_level),
     parent_ref as (select genre_id as id, parent_genre as parent
                    from genre_parent
                    where genre_id in (select id from genre_top_level)),
     parents as (select parent_ref.id as id, genre.id as parent_id, genre.name as parent_name
                 from genre,
                      parent_ref
                 where genre.id = parent_ref.parent),
     child_ref as (select parent_genre as id, genre_id as child
                   from genre_parent
                   where parent_genre in (select id from genre_top_level)),
     children as (select child_ref.id as id, genre.id as child_id, genre.name as child_name
                  from genre,
                       child_ref
                  where genre.id = child_ref.child)
select g.id                              as "id",
       g.name                            as "name",
       g.description                     as "description",
       coalesce((select array_agg(jsonb_build_object('ref', parent_id, 'name', parent_name))
                 from parents
                 where id = g.id), '{}') as "parents",
       coalesce((select array_agg(jsonb_build_object('ref', child_id, 'name', child_name))
                 from children
                 where id = g.id), '{}') as "children"
from genre_top_level as g
group by g.id, g.name, g.description
order by g.name;

-- migrate:down

drop view genre_top_level;

alter table genre
    drop top_level;

alter table genre_parent
    drop constraint parent_genre_pk;

alter table genre_parent
    drop constraint if exists parent_genre_id_parent_u;
