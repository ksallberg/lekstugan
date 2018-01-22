-module(apa).

-compile(export_all).

go(In) ->
    crypto:start(),
    En = crypto:block_encrypt(aes_cfb128,
                              <<"abcdefghabcdefgh">>,
                              <<"12345678abcdefgh">>,
                              In),
    B64 = base64:encode(En),
    HtmlEncode = http_uri:encode(binary_to_list(B64)),
    io:format("Encrypted: ~p HTML encoded: ~p ~n", [B64, HtmlEncode]),
    HtmlUnencode = list_to_binary(http_uri:decode(HtmlEncode)),
    UnB64 = base64:decode(HtmlUnencode),
    UnEn = crypto:block_decrypt(aes_cfb128,
                                <<"abcdefghabcdefgh">>,
                                <<"12345678abcdefgh">>,
                                UnB64),
    io:format("Unencrypted: ~p~n", [UnEn]).
