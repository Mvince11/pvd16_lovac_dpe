/*
Script d'analyse des logements vacants à Ajaccio
Mars 2025
Nicolas Pelé
*/

-- Etape 1 : création de la table zz_ajaccio_lgt.lgt_vac_ort

-- a) On ne garde que les lgt vacants dans le périmètre de l'ORT
drop table if exists zz_ajaccio_lgt.lgt_vac_ort;
create table zz_ajaccio_lgt.lgt_vac_ort as
select 
	vac.*
from zz_ajaccio_lgt.lgt_vac as vac
left join zz_ajaccio_lgt.zone_interet as zi
on st_intersects(vac.geom,zi.geom)
where zi.libelle  = 'ORT Ajaccio'

-- b) table du périmètre centre_ancien
drop table if exists zz_ajaccio_lgt.centre_ancien;
create table zz_ajaccio_lgt.centre_ancien as
select 
	zi.*
from zz_ajaccio_lgt.zone_interet as zi
where zi.libelle  = 'Centre-ancien'


-- c) on rajoute l'info si dans le périmètre du centre-ancien
drop table if exists zz_ajaccio_lgt.lgt_vac_ort_2;
create table zz_ajaccio_lgt.lgt_vac_ort_2 as
select 
	vac.*,
	case 
		when zi.libelle = 'Centre-ancien' then 'oui' 
		else 'non'
	end as centre_ancien,
	case
		when ff_jannath > 2012 then '5_Plus2013'
		when ff_jannath > 1974 then '4_1975_2012'
		when ff_jannath > 1960 then '3_1961_1974'
		when ff_jannath > 1944 then '2_1945_1960'
		when ff_jannath < 1945 then '1_Avant1945'
		else '0_erreur'
	end as periode_construction,
	case
		when statut1 = '0' then 'Plus vacant'
		when statut1 = '1' then 'Est vacant'
		when statut1 = '-1' then 'Non vacant'
		when statut1 = '-2' then 'Probablement non vacant'
		else 'non vérifié'
	end as statut1_lib
		
from zz_ajaccio_lgt.lgt_vac_ort as vac
left join zz_ajaccio_lgt.centre_ancien as zi
on st_intersects(vac.geom,zi.geom)

-- d) on rajoute l'idprocpte, pour ensuite avoir l'info du nbre de droits de propriété
drop table if exists zz_ajaccio_lgt.lgt_vac_ort_3;
create table zz_ajaccio_lgt.lgt_vac_ort_3 as
select 
	vac.*,
	loc.idprocpte
from zz_ajaccio_lgt.lgt_vac_ort_2 as vac
left join ff2024_dep.d2a_fftp_2024_pb0010_local as loc
on vac.ff_idlocal = loc.idlocal

drop table if exists zz_ajaccio_lgt.lgt_vac_ort_tempoproprio;
create table zz_ajaccio_lgt.lgt_vac_ort_tempoproprio as
select 
	proprio.idprocpte,
	count(*) as nbre_droits
from ff2024_dep.d2a_fftp_2024_proprietaire_droit as proprio
group by idprocpte

drop table if exists zz_ajaccio_lgt.lgt_vac_ort_4;
create table zz_ajaccio_lgt.lgt_vac_ort_4 as
select 
	vac.*,
	proprio.nbre_droits
from zz_ajaccio_lgt.lgt_vac_ort_3 as vac
left join zz_ajaccio_lgt.lgt_vac_ort_tempoproprio as proprio
on vac.idprocpte = proprio.idprocpte


-- Etape 2 : analyse rapide des logements de l'ORT
-- 720 lgt vacants
select 
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')

-- a) En fonction du type de biens
select 
	type,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
group by type
order by count desc

-- b) En fonction du type de propriétaire
select 
	ff_catpro2txt,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
group by ff_catpro2txt
order by count desc


-- c) En fonction de la durée de la vacance
select 
	dureevacance,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt != 'ORGANISME DE LOGEMENT SOCIAL'
group by dureevacance
order by dureevacance

-- 328
select 
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE') and
	dureevacance > 4
	
-- 280
select 
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE') and
	dureevacance < 5

-- d) En fonction de la vérification terrain
-- 10 non vacants
-- 23 probablement non vacants
-- 40 vacants
select 
	statut1_lib,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
group by statut1_lib

-- e) En fonction de la typo de lgt vacants
select 
	typo_vac,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
group by typo_vac

-- f) En fonction de l'année de construction
select 
	ff_jannath,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
group by ff_jannath

select 
	periode_construction,
	count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where ff_catpro2txt IN ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
group by periode_construction
order by periode_construction

-- Etape 3 : quantification des situations types 

-- 582 lgt vacants
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	ff_catpro2txt in ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')

-- 16 + 148 + 32 + 9 + 125 = 330 
-- 330/582 = 57%
	
-- le reste sont principalement des lgts hors centre-anciens	
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	centre_ancien = 'non' and
	ff_catpro2txt in ('PERSONNE PHYSIQUE','SOCIETE CIVILE A VOCATION IMMOBILIERE')
	
/* Situation 1 : 
Le logement appartient à une indivision familiale dont les membres sont dispersés (y compris sur le continent) sont connus mais difficiles à mobiliser pour les prises de décisions
L’état général du bâti, tant pour les parties communes que pour les locaux individuels, est dégradé, nécessitant des travaux de grande ampleur pour envisager une remise sur le marché dans des conditions acceptables.
Situé dans un secteur dynamique, au cœur d’un centre ancien dense, ce logement est soumis à une forte pression liée à la transformation des habitations permanentes en meublés touristiques, ces derniers offrant une rentabilité attendue généralement plus élevée pour les propriétaires.
*/ 
	
-- 16
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	nbre_droits > 2 and
	centre_ancien = 'oui' and
	periode_construction in ('1_Avant1945','2_1945_1960','3_1961_1974')

/* Situation 2 : 
Le logement appartient à un unique propriétaire identifié, qui se montre disposé à entreprendre les travaux nécessaires pour remettre son bien sur le marché. Cependant, ses faibles revenus constituent un frein majeur à la réalisation de ces travaux, limitant sa capacité financière à intervenir.
Le bâti, bien que globalement en état « moyen », présente des signes de négligence en raison d’un manque d’entretien. Des travaux restent indispensables pour garantir une remise en état satisfaisante et conforme aux attentes du marché.
Situé dans un secteur dynamique et attractif, ce logement bénéficie d’un fort potentiel d’intérêt pour les habitants, ce qui en fait un bien stratégique à réhabiliter rapidement.
*/ 

-- 148
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	typo_vac in ('1') and -- personne physique
	nbre_droits < 3 and
	centre_ancien = 'oui' and
	periode_construction in ('1_Avant1945','2_1945_1960','3_1961_1974')
	
/* Situation 3 : 
Le logement est détenu par une SCI familiale créée dans une logique patrimoniale. Les associés se montrent réticents à louer le bien, invoquant des contraintes administratives et le risque de dégradations ou d’impayés. Leur réflexion coûts/bénéfices s’oriente vers un calcul qui penche en faveur d’une éventuelle plus-value à la revente plutôt que d’une mise en location.
Le bâti est en bon état et ne nécessite pas de travaux significatifs, facilitant une éventuelle transaction ou une mise sur le marché.
Le logement est situé dans un secteur relativement éloigné du centre-ville et de ses principaux équipements, ce qui peut limiter son attractivité pour certains profils de locataires.
*/ 

-- 32
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	ff_catpro2txt = 'SOCIETE CIVILE A VOCATION IMMOBILIERE' and
	centre_ancien = 'non'

/* Situation 4 : 
Le petit logement est en situation de vacance administrative, le « nouveau » propriétaire restant inconnu depuis le décès du dernier propriétaire identifié il y a une trentaine d’années. Cette absence de gestionnaire complique toute possibilité de réhabilitation ou de remise sur le marché.
Les parties communes de l’immeuble sont en bon état général, mais l’appartement lui-même nécessite des travaux, notamment pour sa mise aux normes énergétiques. Cela représente un frein à sa valorisation immédiate.
Idéalement situé dans un secteur dynamique et attractif, ce logement dispose d’un fort potentiel, mais l’absence de propriétaire identifiable bloque toute perspective d’exploitation ou de valorisation.
*/ 

-- 9
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	typo_vac in ('8','12') and -- succession ou bien présumé sans maitre
	centre_ancien = 'oui'
	
/* Situation 5 : 
Le logement appartient à un propriétaire identifié, mais ce dernier fait face à de graves difficultés financières. Il est actuellement locataire d’un logement social.
L’appartement, ainsi que l’ensemble de l’immeuble, est dans un état très dégradé. L’immeuble est même sous le coup d’une procédure de péril, nécessitant des interventions lourdes pour garantir la sécurité et la salubrité des lieux.
Situé dans un secteur « intermédiaire » relativement éloigné du centre-ville, le logement bénéficie d’un attrait limité, mais pourrait retrouver de la valeur avec une réhabilitation complète.
*/ 

-- 125
select count(*)
from zz_ajaccio_lgt.lgt_vac_ort_4
where 
	statut1_lib not in ('Plus vacant','Non vacant','Probablement non vacant') and
	typo_vac = '1' and -- personne physique
	centre_ancien = 'non' and
	periode_construction in ('1_Avant1945')
	
	
	
	
	
-- AMP
	
drop table if exists zz_ajaccio_lgt.lgtvacants_AMP_codeinsee;
create table zz_ajaccio_lgt.lgtvacants_AMP_codeinsee AS
select 
	lgtvac.*,
	loc.idcom
from zz_ajaccio_lgt.lgtvacants_AMP as lgtvac
left join ff2024.fftp_2024_pb0010_local as loc
on lgtvac.ff_idlocal = loc.idlocal
