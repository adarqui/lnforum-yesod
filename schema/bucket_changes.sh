columns="max_rounds training_styles time_limit num_know num_dont_know num_dont_care num_protest honor_know honor_dont_know honor_dont_care honor_protest boolean_know boolean_dont_know boolean_dont_care boolean_protest match_know match_dont_know match_dont_care match_protest subs_know subs_dont_know subs_dont_care subs_protest splits_know splits_dont_know splits_dont_care splits_protest"

other="threshold"

boom_bucket () {
  cols=$1
  for i in $cols; do
    psql leuro -c "ALTER TABLE bucket ADD COLUMN \"$i\" INT8;"
  done

  for i in $cols; do
    psql leuro -c "UPDATE bucket set $i=0;"
  done

  for i in $cols; do
    psql leuro -c "ALTER TABLE bucket ALTER COLUMN \"$i\" SET NOT NULL;"
  done
}

boom_bucket $columns;
boom_bucket $other;
