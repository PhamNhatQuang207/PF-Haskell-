data Client = Client {
    nom :: String,
    prenom :: String,
    email :: String,
    addresse :: String
}

c1 :: Client
c1 = Client { nom = "Dupont", prenom = "Jean", email = "jean.dupont@example.com", addresse = "123 Rue de Paris" }
c2 :: Client
c2 = Client "Dupont" "Marie" "marie.dupont@example.com" "456 Rue de Lyon"

chContact1 :: String -> String -> Client -> Client
chContact1 newEmail newAddresse client = client { email = newEmail, addresse = newAddresse }

chContact2 :: String -> String -> Client -> Client
chContact2 newEmail newAddresse (Client n p _ _) = Client n p newEmail newAddresse


