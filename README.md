# Thesis Analysis Code
R code used for data exploration, statistical analyses, and visualizations.
The powerpoint presentation (simplified for easy printing) can be found here:
https://s3-us-west-2.amazonaws.com/uiucthesis/SAnderson_Defense_simple_print.pdf

**Abstract**
Plant parasitic nematodes are responsible for food losses worth $78 billion worldwide.
The damage caused by Heterodera glycines, or soybean cyst nematode (SCN), to the US
soybean crop is estimated up to $2 billion annually making it the most destructive soybean
pathogen. SCN is a microscopic obligate endoparasite with an entirely belowground
life history that includes a robust dormant phase. These aspects of SCN biology make
it nearly impossible to eradicate and expensive to manage. The most widely deployed
strategy to manage SCN is an integrated management plan (IPM) that combines non-host
crop rotations (frequently with corn) and planting SCN-resistant cultivars. SCN cyst
diapause–which allows SCN to persist in the soil for several years–limits the effectiveness
of alterate-year crop rotations. Furthermore, evidence that the most widely used resistant
lines are losing their effectiveness is mounting. Additionally, nematicides are increasingly
unavailable due to environmental regulations and also have the disadvantages of being
prohibitively expensive, and performing inconsistently.

Biological control (biocontrol) has recently been seen as a promising addition to IPM.
Viruses are relatively more stable and therefore more persistent in soil, they do not need
to form complicated infection structures, they can be cultured on a commercial scale, and
they have simpler genomes which are amenable to genetic engineering. Furthermore,
viruses exhibit higher host and tissue-specificity, decreasing the risk for non-target effects
seen in other non-virus-based biocontrol cases. Though these properties of viruses
have been demonstrated in insect pests with favorable results, viruses have not been
investigated for nematode biocontrol. This is because infectious virus in nematodes had
gone unnoticed until a couple of recent discoveries. At the University of Illinois, five SCN
ssRNA viruses were discovered in 2011 giving us ample opportunity–paving the way
to explore their potential as biocontrol agents. While empirical data on the pathologies
of these viruses emerges, we opted to use a computational approach to investigate
pathotypic factors needed for desirable nematode suppression, the epizootiology of
within-nematode evolution, and the long-term population-level behavior.
This study used an agent-based model, SCNSim, to simulate a virus-nematode-soybean
system to investigate within-host virulence evolution. The nematode-agent recapitulates
the nematode life cycle and uses purely stochastic events to advance each transition in the
model. SCNSim was used to test a range of mutation rates, initial virulences, and release
strategies. This investigation uses weather data and soybean planting and harvesting
patterns in Champaign, IL. Where empirical data is lacking dimensionless parameters
and probabilities were used.

Results of the simulation showed viruses inoculated at 80% prevalence caused significantly
more mortalities than those inoculated at 20% prevalence. Further investigation
revealed the low mortality under lower prevalence was likely due to high horizontal and
vertical transmission leading to rapid thinning of the overall viral burden as the disease
spread to nearly 100% of the population. Mortality rate was found to be dependent on
virulence and fidelity. Pathotypes with high starting virulence resulted in premature
peaked mortalities which suggested shorter lifetime transmission. Furthermore, virulence
and fidelity were inversely related in pathotypes with the highest mortalities. Further,
these pathotypes had population-level fitnesses near the error threshold between persistence
or extinction. Qualitative stability analysis revealed medial pathotypes exhibited
stable long-term behavior reminiscent of a spiral sink in the mortality and transmissibility
phase space.

That intermediately virulent and transmissible viruses lead to the most damaging
epizootics is in agreement with the generally accepted trade-off theory in virulence
evolution. However, the evolution of the transmissibility and prevalence curves through
time reveal some inconsistencies in the mechanisms of disease spread with respect to
theory that require further testing. Testing combinations of varying levels of transmission
rates, and virus loads for instance can provide insight on how different transmission
modes impact virulence evolution and the efficacy of nematode suppression. There are
still many areas where SCNSim can be improved–particularly when the transmission
rates and dominant transmission modes of the SCN RNA viruses becomes known. In
its present state, SCNSim has predicted the attenuation of nematode mortalities within
the first four crop seasons due to either a dilution of virus particles thoughout the
population or the insulation of the infected population resulting from mortality rates
exceeding transmission rates. The goal for the near future is to apply SCNSim alongside
genetic engineering and experimental testing of the SCN viruses, in a decision-making
framework for virus-agent deployment strategies and improving virus-agent efficacy
within an evolutionary timescale for nematode biocontrol.
