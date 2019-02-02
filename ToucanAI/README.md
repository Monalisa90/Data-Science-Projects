Using Spacy & NLTK, building a question classifier based on heuristics. I am using 4 conditions:

1) If first word of the sentence is any of the interogative words, then it is a question
   Eg.- 'How many Medieval Warm Period reconstructions were used'

2) If the sentence contains '?' mark, then it is a question
   Eg.- 'Who announced she would step down as leader of the Conservatives?'

3) If the second word or the last word of the sentence is any of the wh_words, then it is a question
   Eg.- 'In what country is PloS Pathogens headquartered'
        'Conservative Islam classifies non-Muslims who follow Shia interpretation as what'

4) If the sentence contains 'what':
   i)  Check for ',' and '.' before 'what' using spacy & pos_tags, if it exists then it is a question
       Eg.- 'Along with solar, coal and nuclear, what sort of plants notable use the Rankine process'
            'Orange, San Diego, Riverside and San Bernardino make up four of the five counties. What is the name of the last county'
   
   ii) Check for prepositions and past tense of verbs before 'what' using spacy & pos_tags
       Eg.- 'Along with mills and mines, in what industrial locations did steam drive cultivation'
            'The abolition of the Ottoman Caliphate is believed to have started what system'
   
Still working on rules for 'which' word, as sometimes the 'which' word creates confusion in an interogative statement and non-intergative 
statement.
