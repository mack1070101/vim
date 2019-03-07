SELECT * FROM owner_account_balance_adjustment oab
JOIN account a on a.id = oab.account_id
ORDER BY a.id DESC ;