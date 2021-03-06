create table feeds (
  feed_id serial primary key,
  feed_url varchar not null unique,
  feed_title varchar,
  feed_link varchar,
  feed_itunes_url varchar,
  feed_description text,
  feed_last_build_date varchar, -- for now
  feed_explicit bool null, 
  feed_keywords varchar null,
  feed_categories varchar,
  feed_categories_ids varchar null, -- for indexing only
  feed_summary text  -- may be redundnat with description
  );

create table items (
  item_id serial primary key,
  feed_id integer not null references feeds (feed_id),
  feed_title varchar not null,  -- denormalization
  feed_categories varchar null, -- copy of feed category int ids
  item_title varchar not null,
  item_link varchar,
  item_summary text,
  item_summary_plain text,
  item_pubdate timestamp with time zone null, 
  item_guid varchar not null,
  item_categories varchar null,
  item_keywords varchar null,
  item_audio_url varchar not null,
  item_duration integer null,  -- seconds
  item_explicit boolean null,
  item_tag_ids varchar null -- multiattr
  );

create unique index items_feed_id_guid_uniq_idx ON items (feed_id, item_guid); 

create table tags (
  tag_id serial primary key,
  tag varchar(51)
  );

create index tag_idx on tags(tag);
