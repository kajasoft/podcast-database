#!/bin/bash
# extract hierarchy

awk -F '"' '{
for (i=1;i<=NF;i++) { 
if (i % 2 == 0) {
  printf("%s\t", $i);
}
}
  printf("\n");

}'
