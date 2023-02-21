#!/bin/bash

sparql --data=../resources/be/vlaanderen/omgeving/data/id/catalog/algemeen-catalog-publiek/catalog.ttl --query ../sparql/catalog_visualisatie.sparql --results=TURTLE | rdf2dot  | dot -Tpng > ../sparql/catalog_visualisatie.png
pushd  ../../..
mvn clean install
unzip target/algemeen-catalog-publiek-1.1.0-SNAPSHOT.jar -d target
find target/be  | grep ttl | xargs riot --formatted=TURTLE > /tmp/catalog.ttl
popd
sparql --data=/tmp/catalog.ttl --query ../sparql/catalog_visualisatie.sparql --results=TURTLE | rdf2dot  | dot -Tpng > ../sparql/volledige_catalog_visualisatie.png


