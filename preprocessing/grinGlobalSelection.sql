SELECT a.accession_number_part1 +' '+ convert(varchar, a.accession_number_part2) +
 COALESCE(' '+ a.accession_number_part3, '') 
 AS accession_number, a.status_code, a.improvement_status_code,
t.topname, ts.name AS 'Taxon', s.site_short_name, src.latitude, src.longitude, src.elevation_meters, src.environment_description, src.collector_verbatim_locality, src.formatted_locality

FROM accession a
JOIN topname t ON a.accession_id=t.accession_id
JOIN taxonomy_species ts ON a.taxonomy_species_id=ts.taxonomy_species_id
JOIN taxonomy_genus tg ON ts.taxonomy_genus_id=tg.taxonomy_genus_id
LEFT JOIN accession_source src ON a.accession_id=src.accession_id
LEFT JOIN geography g on src.geography_id=g.geography_id
join cooperator ac ON a.owned_by = ac.cooperator_id
join site s ON ac.site_id=s.site_id
where src.is_origin='y'
and tg.genus_name IN ('Vitis')
and g.country_code IN ('USA', 'CAN', 'MEX')
