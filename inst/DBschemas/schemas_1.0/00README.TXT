This directory contains version 1.0 of the full set of schemas used in the
sqlite-based annotation data packages.

Version 1.0 is our "ideal" target for the BioC 2.1 release but we might
not have enough time and resources to make this happen (because of other
priorities) so we might just stick to version 0.9.

Version 1.0 has been successfully tested (i.e. imported) with SQLite
(3.4.1), MySQL+InnoDB (5.0.26) and PostgreSQL (8.1.9) on a 64-bit openSUSE
10.2 system. It has not been tested on Oracle yet.

All the *.sqlite files using one of the 1.0 schemas must set DBSCHEMAVERSION
to 1.0 in their "metadata" table.

See the DataTypes.txt file for all the data types used across the 1.0 schemas.


SUMMARY OF CHANGES SINCE VERSION 0.9
------------------------------------

Note that all those changes are disruptive (i.e. they potentially break the
SQL queries written for version 0.9).

All schemas:

  o renamed "qcdata" table -> "map_counts"

All probe-based and org-based schemas:

  o renamed internal id "id" -> "_id"
  o renamed chromosome_locations.chromosome -> chromosome_locations.seqname
  o renamed kegg.kegg_id col -> kegg.path_id
  o renamed chrlengths.chr col -> chrlengths.chromosome

All probe-based schemas:

  o moved "_id" col to last position in "probes" table
  o removed the "accessions" table

GO_DB schema:

  o renamed "term_id" cols -> "_id"
  o renamed "offspring_id" cols -> "_offspring_id"
  o renamed "parent_id" cols -> "_parent_id"
  o renamed "evidence" cols -> "relationship_type"
  o removed "gene2go_evidence" table
  o moved "ontology" col to 1st position in "go_ontology" table
  o removed go_obsolete.term_id column and made go_obsolete.go_id the
    new PRIMARY KEY

  Note that, ideally, we could add UNIQUE constraints on (_id, _parent_id) in
  the go_[bp|cc|mf]_parents tables and on (_id, _offspring_id) in the
  go_[bp|cc|mf]_offspring tables but the cost of doing this is high (+5M for
  the size of GO.sqlite: 37M instead of 32M).

KEGG_DB schema:

  o renamed "gene_id" col -> "gene_or_orf_id"




SUMMARY OF CHANGES BETWEEN VERSION .9 and .91 (PRE SQL update)
---------------------------------------------

RODENT and HUMAN schemas:

  o added ensembl table to all RODENT and HUMAN schemas

FLY schemas:

  o added flybase_cg table to all FLY schemas
