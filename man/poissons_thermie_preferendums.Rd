\name{poissons_thermie_preferendums}
\alias{poissons_thermie_preferendums}
\docType{data}
\title{
Jeu de données de préférendums thermiques d'espèces piscicoles dulcicoles
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("poissons_thermie_preferendums")}
\format{
  Un tibble de données relatif à différens préférendums thermiques d'espèces piscicoles dulcicoles, contenant les champs suivants :
  \describe{
    \item{\code{codeespece}}{le code espèce à 3 lettres proposés par le CEMAGREF}
    \item{\code{nomfrancais}}{le nom d'espèce courant, en français}
    \item{\code{nomlatin}}{le nom latin de l'espèce}
    \item{\code{value}}{la valeur de température}
    \item{\code{seuil}}{le type de seuil concerné (\code{Optimal_min}, \code{Optimal_max} ou \code{Resistance_max})}
    \item{\code{stade}}{le type de stade concerné (\code{Embryon}, \code{Larve}, \code{Juvénile}, \code{Adulte} ou \code{Reproduction})}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
  \itemize{
    \item{Pour les espèces "BAF", "GRE", "PER", "BLN", "BRO", "ABL", "VAN", "TOX", "HOT", "PES", "CHE", "GAR", "SIL", "SAN", "GOU", "BOU", "SPI", "BRB", "BRE" : Souchon, Y., et L. Tissot. « Synthesis of thermal tolerances of the common freshwater fish species in large Western Europe rivers ». Knowledge and Management of Aquatic Ecosystems, nᵒ 405 (19 juillet 2012): 03. \url{https://doi.org/10.1051/kmae/2012008}.}
    \item{Pour les espèces "TRF", "OBR", "VAI" : Elliott, J. M., éd. « Some aspects of thermal stress on freshwater Teleosts ». In Stress and fish, 209‑45. London, Royaume-Uni, 1981.}
    \item{Pour l'espèce "CHA" : Andreasson, S. « Feeding habits of a sculpin (Cottus gobio L. Pisces) population ». Rep. Inst. Freshwat. Res. Drottningholm 51 (1971): 5‑30. & Elliott, J.m., et J.a. Elliott. « The Critical Thermal Limits for the Bullhead, Cottus Gobio, from Three Populations in North-West England ». Freshwater Biology 33, nᵒ 3 (1995): 411‑18. \url{https://doi.org/10.1111/j.1365-2427.1995.tb00403.x}. IN Dorts, Jennifer, Gaël Grenouillet, Jessica Douxfils, Syaghalirwa N. M. Mandiki, Sylvain Milla, Frédéric Silvestre, et Patrick Kestemont. « Evidence That Elevated Water Temperature Affects the Reproductive Physiology of the European Bullhead Cottus Gobio ». Fish Physiology and Biochemistry 38, nᵒ 2 (1 avril 2012): 389‑99. \url{https://doi.org/10.1007/s10695-011-9515-y}.}
    \item{Pour l'espèce "LOF" : Elliott, J.m., J.a. Elliott, et J.d. Allonby. « The Critical Thermal Limits for the Stone Loach, Noemacheilus Barbatulus, from Three Populations in North-West England ». Freshwater Biology 32, nᵒ 3 (1994): 593‑601. \url{https://doi.org/10.1111/j.1365-2427.1994.tb01150.x}.}
    }
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(poissons_thermie_preferendums)
PoissonsPreferendumsThermiques \%>\% view()
}
\keyword{datasets}
