use strict;
use warnings;

sub vars {
  my @as;
  for (my $a = 'a', my $i = 0; $i < $_; ++$a, ++$i) {
    push @as, $a;
  }
  @as
}

if (@ARGV != 1) {
  die 'Usage: perl Rows.perl src/Database/PostgreSQL/Row.purs';
}

open my $out, '>', $ARGV[0]
  or die $!;

print $out <<'EOF';
module Database.PostgreSQL.Row where

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foreign (Foreign)
import Database.PostgreSQL.Value (class FromSQLValue, class ToSQLValue, fromSQLValue, toSQLValue)
import Prelude

-- | Convert things to SQL rows.
class ToSQLRow a where
    toSQLRow :: a -> Array Foreign

-- | Convert things from SQL rows.
class FromSQLRow a where
    fromSQLRow :: Array Foreign -> Either String a
EOF

for (0 .. 19) {
  print $out "\n";

  print $out "-- | A row with $_ field" . ($_ == 1 ? '' : 's') . ".\n";
  print $out "data Row$_";
  print $out map { " $_" } vars($_);
  print $out " = Row$_";
  print $out map { " $_" } vars($_);

  print $out "\n\n";

  if ($_ == 0) {
    print $out "derive instance eqRow$_ :: Eq Row$_";
  } else {
    print $out "derive instance eqRow$_ :: (";
    print $out join(', ', map { "Eq $_" } vars($_));
    print $out ") => Eq (Row$_" . join('', map { " $_" } vars($_)) . ")";
  }

  print $out "\n\n";

  if ($_ == 0) {
    print $out "derive instance ordRow$_ :: Ord Row$_";
  } else {
    print $out "derive instance ordRow$_ :: (";
    print $out join(', ', map { "Ord $_" } vars($_));
    print $out ") => Ord (Row$_" . join('', map { " $_" } vars($_)) . ")";
  }

  print $out "\n\n";

  if ($_ == 0) {
    print $out "instance showRow$_ :: Show Row$_";
  } else {
    print $out "instance showRow$_ :: (";
    print $out join(', ', map { "Show $_" } vars($_));
    print $out ") => Show (Row$_" . join('', map { " $_" } vars($_)) . ')';
  }
  print $out " where\n";
  if ($_ == 0) {
    print $out "  show Row$_ =\n";
    print $out "    \"Row$_\"";
  } else {
    print $out "  show (Row$_" . join('', map { " $_" } vars($_)) .  ") = \n";
    print $out "    \"(Row$_ \" <> ";
    print $out join(' <> " " <> ', map { "show $_" } vars($_));
    print $out " <> \")\"";
  }

  print $out "\n\n";

  if ($_ == 0) {
    print $out "instance fromSQLRowRow$_ :: FromSQLRow Row$_";
  } else {
    print $out "instance fromSQLRowRow$_ :: (";
    print $out join(', ', map { "FromSQLValue $_" } vars($_));
    print $out ") => FromSQLRow (Row$_" . join('', map { " $_" } vars($_)) . ')';
  }
  print $out " where\n";
  print $out '  fromSQLRow [' . join(', ', vars($_)) . "] =\n";
  print $out "    pure Row$_\n";
  for (vars($_)) {
    print $out "    <*> fromSQLValue $_\n";
  }
  print $out '  fromSQLRow xs = Left $ "Row has " <> show n <> " fields,';
  print $out " expecting $_.\"\n";
  print $out '    where n = Array.length xs';

  print $out "\n\n";

  if ($_ == 0) {
    print $out "instance toSQLRowRow$_ :: ToSQLRow Row$_";
  } else {
    print $out "instance toSQLRowRow$_ :: (";
    print $out join(', ', map { "ToSQLValue $_" } vars($_));
    print $out ") => ToSQLRow (Row$_" . join('', map { " $_" } vars($_)) . ')';
  }
  print $out " where\n";
  if ($_ == 0) {
    print $out "  toSQLRow Row$_ = []";
  } else {
    print $out "  toSQLRow (Row$_" . join('', map { " $_" } vars($_)) .  ") = \n";
    print $out '    [' . join(', ', map { "toSQLValue $_" } vars($_)) . ']';
  }
}
