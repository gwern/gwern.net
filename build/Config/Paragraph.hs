module Config.Paragraph where

minLength :: Int
minLength = 768

-- testing: unique list, all URLs
whitelist :: [String]
whitelist = ["/doc/economics/1998-delong.pdf", "/doc/cs/algorithm/1980-rytter.pdf"
            , "https://cloud.google.com/storage/docs/gsutil/commands/config", "https://terrytao.wordpress.com/wp-content/uploads/2010/10/cosmic-distance-ladder.pdf"
            , "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC526783/", "https://www.strml.net/"
            , "https://antilop.cc/sr/files/Silk_Road_JTAN_com_Search_Warrant.pdf", "https://publicdomainreview.org/essay/the-snowflake-man-of-vermont/"
            , "https://www.reddit.com/r/AIDungeon/", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4183313/bin/pnas.1404623111.sapp.pdf"
            , "/doc/design/typography/1986-koved.pdf", "https://arxiv.org/abs/hep-ph/0204295"
            , "/doc/statistics/meta-analysis/2013-couzinfrankel.pdf", "https://lllyasviel.github.io/DanbooRegion/paper/paper.pdf"
            , "https://journals.plos.org/plosmedicine/article/info%3Adoi%2F10.1371%2Fjournal.pmed.1000245", "https://www.kickscondor.com/"
            , "https://personal.math.ubc.ca/~cass/Euclid/byrne.html", "/doc/philosophy/epistemology/2023-habgoodcoote.pdf"
            , "https://gscan2pdf.sourceforge.net/", "https://arxiv.org/abs/math/0604049"
            , "https://arxiv.org/abs/1406.1077", "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC557238/"
            , "https://transformer-circuits.pub/2021/framework/index.html#anthropic", "https://arxiv.org/abs/astro-ph/9912202"
            , "https://arxiv.org/abs/math/0512268", "https://arxiv.org/abs/chao-dyn/9406003"
            , "https://arxiv.org/abs/0905.3590", "https://arxiv.org/abs/1302.2898"
            , "https://arxiv.org/abs/hep-ph/9306225", "/doc/cs/2017-denning.pdf"
            , "/doc/cs/hardware/2015-brooks.pdf", "/doc/genetics/heritable/2018-vogel.pdf"
            , "/doc/genetics/heritable/2019-kaiser.pdf", "/doc/genetics/selection/artificial/2023-meyer-supplement-science.ade1083_sm.pdf"
            , "/doc/genetics/selection/natural/2022-walsh.pdf", "/doc/psychiatry/autism/2024-schindel.pdf"
            , "/doc/psychology/neuroscience/2023-dunn.pdf", "/doc/reinforcement-learning/robot/2020-won.pdf"
            , "/doc/sociology/2022-ullman.pdf", "/doc/sociology/technology/2022-wang-8.pdf"
            , "/doc/statistics/decision/1990-dantzig.pdf"]
