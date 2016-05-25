create table feeds (
  feed_id serial primary key,
  feed_title varchar,
  feed_link varchar,
  feed_description text,
  feed_last_build_date varchar, -- for now
  feed_explicit varchar, -- switch to bool
  feed_keywords varchar,
  feed_categories varchar,
  feed_summary text  -- may be redundnat with description
  );
